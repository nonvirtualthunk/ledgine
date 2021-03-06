package arx.engine.world

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/15/18
  * Time: 1:20 PM
  */

import arx.core.introspection.{Clazz, Field}
import arx.engine.data.TAuxData
import arx.engine.event.GameEvent
import org.scalatest.FlatSpec

case class FooData(
	var a : Int = 0,
	var b : Float = 0.0f,
	var nested : Nested = new Nested,
	var nestedMap : Map[AnyRef, Nested] = Map()) extends TAuxData {
	def this() {
		this(0,0.0f,new Nested,Map())
	}
}

case class Nested() {
	var x : Int = 1
	var y : Int = 1
}

object FooData extends Clazz[FooData]("FooData", classOf[FooData]) {
	val Sentinel = new FooData
	val a = Field.fromValue(Sentinel.a).createField[FooData]("i",f => f.a, (f,i) => f.a = i, FooData)
	fields += "a" -> a
	val b = Field.fromValue(Sentinel.b).createField[FooData]("s",f => f.b, (f,s) => f.b = s, FooData)
	fields += "b" -> b
	val nested = Field.fromValue(Sentinel.nested).createField[FooData]("nested", f => f.nested, (f,nested) => f.nested = nested, FooData)
	fields += "nested" -> nested
	val nestedMap = Field.fromValue(Sentinel.nestedMap).createField[FooData]("nestedMap", f => f.nestedMap, (f,nestedMap) => f.nestedMap = nestedMap, FooData)
	fields += "nestedMap" -> nestedMap

	override def copyInto(from: FooData, to: FooData): Unit = {
		to.a = from.a
		to.b = from.b
		to.nested = from.nested
		to.nestedMap = from.nestedMap
	}
}

object Nested extends Clazz[Nested]("Nested", classOf[Nested]) {
	val Sentinel = new Nested
	val x = Field.fromValue(Sentinel.x).createField[Nested]("x", f => f.x, (f,x) => f.x = x, Nested)
	fields += "x" -> x
	val y = Field.fromValue(Sentinel.y).createField[Nested]("y", f => f.y, (f,y) => f.y = y, Nested)
	fields += "y" -> y

	override def copyInto(from: Nested, to: Nested): Unit = {
		to.x = from.x
		to.y = from.y
	}
}

case class TestEvent(i : Int) extends GameEvent {

}

class WorldTest extends FlatSpec {
	import arx.core.introspection.FieldOperations._

	"Ledger world" should "be able to add data and retrieve it from views" in {
		val world = new World

		world.register[FooData]()

		val entity = world.createEntity()

		world.attachData(entity, new FooData)

		implicit val view = world.view

		val foo = entity[FooData]

		assert(foo.a == 0)
		assert(foo.b == 0.0f)

		world.modify(entity, FooData.a + 3, "test source")
		world.endEvent(TestEvent(0))
		assert(world.currentTime == GameEventClock(0))

		assert(foo.a == 3)

		val entity2 = world.createEntity()

		world.attachData(entity2, new FooData)

		val foo2 = entity2[FooData]

		assert(foo2.a == 0)

		world.modify(entity2, FooData.b + 1.0f, "test source")
		world.endEvent(TestEvent(1))

		assert(foo2.b == 1.0f)

		val viewAt1 = world.viewAtTime(world.currentTime - 1)

		val foo2At1 = viewAt1.dataOpt[FooData](entity2)

		val viewAt2 = world.viewAtTime(world.currentTime)

		val foo2At2 = viewAt2.dataOpt[FooData](entity2)

		assert(viewAt1.wrappedEvents.size == 1)
		assert(foo2At1.isEmpty)
		assert(foo2At2.exists(foo => foo.b == 1.0f))
	}
	
	"Ledger world" should "be able to support nested field modifications" in {
		val world = new World

		world.register[FooData]()

		val entity = world.createEntity()
		val baseData = new FooData
		baseData.nestedMap = Map("test1" -> new Nested, "test2" -> new Nested)
		world.attachData(entity, baseData)

		implicit val view : WorldView = world.view

		val foo = entity[FooData]

		assert(foo.nested == Nested.Sentinel)
		val oldNestedRef = foo.nested

		world.modify(entity, NestedModifier(FooData.nested, Nested.x + 3), "test source")
		world.addEvent(TestEvent(1))

		assert(foo.nested.x == 4)
		assert(oldNestedRef.x == 1)

		assert(foo.nestedMap("test1").x == 1)

		world.modify(entity, NestedKeyedModifier(FooData.nestedMap, "test1", Nested.y + 1), "test source")
		world.addEvent(TestEvent(2))

		assert(foo.nestedMap("test1").y == 2)
		assert(foo.nestedMap("test2").y == 1)
	}

	"Ledger world data modification log" should "contain an accurate reflection of the changes made to a data class" in {
		val world = new World

		world.register[FooData]()

		val entity = world.createEntity()
		world.attachDataWith[FooData](entity, f => f.a = 1)

		implicit val view = world.view

		world.modify(entity, FooData.a + 1, "modification 1")
		world.modify(entity, FooData.b -> 3, "modification 2")

		world.addEvent(TestEvent(1))

		world.modify(entity, FooData.a - 3, "modification 3")

		world.addEvent(TestEvent(2))

		val modificationLog = world.dataModificationLog[FooData](entity)
		assert(modificationLog.finalValue.a == -1)
		assert(modificationLog.finalValue.b == 3)
		val breakdown = modificationLog.breakdownFor(FooData.a, "base a")
		// this one should have 3 breakdown elements, 1 from the non-zero initial state, two from modifications
		assert(breakdown.elements.size == 3)
		assert(breakdown.elements(0).impact == Impact.Neutral)
		assert(breakdown.elements(0).source.contains("base a"))

		assert(breakdown.elements(1).impact == Impact.Positive)
		assert(breakdown.elements(1).source.contains("modification 1"))

		assert(breakdown.elements(2).impact == Impact.Negative)
		assert(breakdown.elements(2).source.contains("modification 3"))

		// this one should only have 1 breakdown element because its base value is 0 and it has one modification
		val breakdownB = modificationLog.breakdownFor(FooData.b, "base b")
		assert(breakdownB.elements.size == 1)

		assert(breakdownB.elements(0).impact == Impact.Positive)
		assert(breakdownB.elements(0).source.contains("modification 2"))
	}

	"world view" should "be able to enable and disable modifications and see the effects reflected" in {
		val world = new World
		world.register[FooData]

		val curView = world.view

		val entity1 = world.createEntity()
		val entity2 = world.createEntity()

		entity1.attach(new FooData(1,1.0f)).in(world)
		entity2.attach(new FooData(2,2.0f)).in(world)

		val t0 = world.addEvent(TestEvent(0))

		val modRef = world.modify(entity1, FooData.a + 3)

		val t1 = world.addEvent(TestEvent(1))

		assert(entity1(FooData)(curView).a == 4)

		world.toggleModification(modRef, enable = false)
		val t2 = world.addEvent(TestEvent(2))

		assert(entity1(FooData)(curView).a == 1)

		world.toggleModification(modRef, enable = true)
		val t3 = world.addEvent(TestEvent(3))

		assert(entity1(FooData)(curView).a == 4)


		val walkView = world.viewAtTime(t0)
		assert(entity1(FooData)(walkView).a == 1)
		world.updateViewToTime(walkView, t1)
		assert(entity1(FooData)(walkView).a == 4)
		world.updateViewToTime(walkView, t2)
		assert(entity1(FooData)(walkView).a == 1)
		world.updateViewToTime(walkView, t3)
		assert(entity1(FooData)(walkView).a == 4)
	}
}
