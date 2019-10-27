package arx.engine.world

import arx.Prelude.toArxAny
import arx.core.introspection.FieldGenerator
import arx.engine.data.{TAuxData, TTestAuxData}
import org.scalatest.FunSuite
import arx.core.introspection.FieldOperations._
import arx.core.macros.GenerateCompanion
import arx.core.metrics.{Metrics, Stopwatch}
import arx.engine.entity.Entity
import arx.engine.event.{GameEvent, WorldCreatedEvent}
import arx.engine.world.EventState.{Ended, Started}
import arx.engine.world.HypotheticalWorldTest.{TestAttackData, TestAttackEvent, TestCreatureData, TestDamageEvent, TestDodgeEvent}

class HypotheticalWorldTest extends FunSuite {

	test("basic hypothetical world functionality") {
		var eventsFromRoot = List[GameEvent]()
		var eventsFromBranch = List[GameEvent]()
		var eventsFromBranchRootCB = List[GameEvent]()

		val root = new World
		val rootEntity = root.createEntity()
		root.attachData(rootEntity, FooData(1))
		root.modify(rootEntity, FooData.a -> 2, None)

		root.onEventCallbacks ::= ((world: World, ge: GameEvent) => {
			if (world eq root) {
				eventsFromRoot ::= ge
			}
			else {
				eventsFromBranchRootCB ::= ge
			}
		})

		val hypo = new HypotheticalWorld(root, root.view)
		hypo.onEventCallbacks ::= ((world: World, ge: GameEvent) => {
			assert(world eq hypo)
			eventsFromBranch ::= ge
		})

		assert(hypo.view.data[FooData](rootEntity).a == 2, "initial view of data should match latest root data")

		hypo.modify(rootEntity, FooData.a -> 3, None)

		assert(hypo.view.data[FooData](rootEntity).a == 3, "modified data should be reflected in fork")
		assert(root.view.data[FooData](rootEntity).a == 2, "modified data should _not_ be reflected in root")

		val hypoEntity = hypo.createEntity()
		assert(hypoEntity.id != rootEntity.id, "id counters should be locked, so hypothetical world created entity should have different id")

		hypo.attachData(hypoEntity, FooData(-1))
		assert(!root.view.entities.exists(e => e == hypoEntity), "entity created in hypothetical world should not bleed down")
		assert(hypo.view.data[FooData](hypoEntity) == FooData(-1), "entity created in hypothetical world should be available in that world's view")

		hypo.modify(hypoEntity, FooData.a -> -2, None)
		assert(hypo.view.data[FooData](hypoEntity) == FooData(-2), "modifications to entities created in hypothetical world should take effect there")
		assert(root.view.dataOpt(hypoEntity).isEmpty, "modifications to entities created in hypothetical world should have no effect on root")

		hypo.pushEvent(TestEvent(1), Ended)

		assert(hypo.view.wrappedEvents.map(_.event) == Vector(TestEvent(1)), "hypothetical world should get events that are added to it")
		assert(root.view.wrappedEvents == Vector(), "hypothetical world events should not be visible to the root")
		assert(eventsFromBranch == List(TestEvent(1)), "hypothetical world callbacks should fire for events added there")
		assert(eventsFromBranchRootCB == List(TestEvent(1)), "hypothetical world events should be seen by root's callbacks, but with hypothetical world parameter")
		assert(eventsFromRoot == List(), "hypothetical world events should not show up in the root with the root's own world as a parameter")
	}


	test("perform hypothetical attack to determine effectiveness") {
		val root = new World


		val attacker = root.createEntity()
		val target = root.createEntity()
		val attack = root.createEntity()
		attack attach TestAttackData(0, 1, 4) in root

		target attach TestCreatureData(10, 10) in root
		attacker attach TestCreatureData(10, 5) in root

		root.addEvent(TestEvent(0))

		{
			val hyp = new HypotheticalWorld(root, root.view)

			HypotheticalWorldTest.performTestAttack(hyp, attacker, target, attack)

			assert(hyp.view.hypotheticalEvents.exists(e => e.state == Ended && e.event == TestAttackEvent(attacker, target, attack, TestAttackData(0, 1, 4))))
			assert(hyp.view.hypotheticalEvents.exists(e => e.event == TestDodgeEvent(target)))
		}
	}

	test("branch hypothetical world off of different historical points") {
		val root = new World

		val attacker = root.createEntity()
		val target = root.createEntity()
		val attack = root.createEntity()
		attack attach TestAttackData(0, 1, 4) in root

		target attach TestCreatureData(10, 10) in root
		attacker attach TestCreatureData(10, 5) in root

		root.addEvent(new WorldCreatedEvent)
		val t1 = root.currentTime

		attacker.modify(Companions.TestCreatureData.hp -> 5, None).in(root)

		root.addEvent(TestEvent(1))
		WorldQuery.assert(s"hp == 5 WHERE id == $attacker")(root.view)

		val t2 = root.currentTime

		attacker.modify(Companions.TestCreatureData.hp -> 1, None).in(root)

		root.addEvent(TestEvent(2))
		val t3 = root.currentTime

		val walkingView = root.viewAtTime(t1)

		val t1HypWorld = new HypotheticalWorld(root, walkingView)
		WorldQuery.assert(s"hp == 10 WHERE id == $attacker")(walkingView)
		WorldQuery.assert(s"hp == 10 WHERE id == $attacker")(t1HypWorld.view)

		t1HypWorld.modify(attacker, Companions.TestCreatureData.hp + 2, None)
		t1HypWorld.addEvent(TestEvent(2))
		WorldQuery.assert(s"hp == 12 WHERE id == $attacker")(t1HypWorld.view)

		root.updateViewToTime(walkingView, t2)
		val t2HypWorld = new HypotheticalWorld(root, walkingView)

		WorldQuery.assert(s"hp == 5 WHERE id == $attacker")(walkingView)
		WorldQuery.assert(s"hp == 5 WHERE id == $attacker")(t2HypWorld.view)

		t2HypWorld.modify(attacker, Companions.TestCreatureData.hp + 2, None)
		t2HypWorld.addEvent(TestEvent(3))
		WorldQuery.assert(s"hp == 7 WHERE id == $attacker")(t2HypWorld.view)

		root.updateViewToTime(walkingView, t3)
		val t3HypWorld = new HypotheticalWorld(root, walkingView)

		WorldQuery.assert(s"hp == 1 WHERE id == $attacker")(walkingView)
		WorldQuery.assert(s"hp == 1 WHERE id == $attacker")(t3HypWorld.view)
	}



}

object HypotheticalWorldTest {

	import arx.core.introspection.FieldOperations._

	def performTestAttack(world : World, attacker : Entity, target : Entity, attack : Entity): Unit = {
		implicit val view = world.view

		world.pushEvent(TestAttackEvent(attacker, target, attack, attack[TestAttackData].copy()), Started)

		val attackData = attack[TestAttackData]

		if (attackData.toHit > 0) {
			for (_ <- 0 until attackData.strikeCount) {
				world.startEvent(TestDamageEvent(target, attackData.strikeDamage))
//				target.modify(Companions.TestCreatureData.hp - attackData.strikeDamage, None).in(world)
				world.endEvent(TestDamageEvent(target, attackData.strikeDamage))
			}
		} else {
			world.addEvent(TestDodgeEvent(target))
		}

		world.pushEvent(TestAttackEvent(attacker, target, attack, attack[TestAttackData].copy()), Ended)
	}

	@GenerateCompanion
	case class TestAttackData(var toHit: Int = 0,
									  var strikeCount: Int = 0,
									  var strikeDamage: Int = 0
									 ) extends TTestAuxData
	object TestAttackData {
		val Sentinel = TestAttackData()
	}

	@GenerateCompanion
	case class TestCreatureData(var hp : Int = 10, var size : Int = 5) extends TTestAuxData

	object TestCreatureData {
		val Sentinel = TestCreatureData()
	}

	case class TestAttackEvent(attacker: Entity, target: Entity, attack: Entity, effectiveAttackData : TestAttackData) extends GameEvent {

	}

	case class TestDamageEvent(entity : Entity, damage : Int) extends GameEvent
	case class TestDodgeEvent(entity : Entity) extends GameEvent

}