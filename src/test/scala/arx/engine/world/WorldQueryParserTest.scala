package arx.engine.world

import arx.engine.world.HypotheticalWorldTest.{TestAttackData, TestCreatureData}
import arx.engine.world.WorldQueryParser.parseExpression
import fastparse.Parsed
import org.scalatest.FunSuite

class WorldQueryParserTest extends FunSuite {


	val world = new World
	world.register[TestAttackData]
	world.register[TestCreatureData]

	implicit val view = world.view

	val entityA = world.createEntity()
	val entityB = world.createEntity()
	val entityC = world.createEntity()
	entityA.attachI(new TestAttackData()) { AD =>
		AD.toHit = 3
	}(world)
	entityA.attachI(new TestCreatureData()) { CD =>
		CD.hp = 10
	}(world)

	entityB.attachI(new TestAttackData()) { AD =>
		AD.toHit = -3
	}(world)

	entityC.attachI(new TestCreatureData()) { CD =>
		CD.hp = 5
	}(world)

	test("parsing") {
		val raw = fastparse.parse("select toHit, hp from TestAttackData, TestCreatureData where hp > 3", parseExpression(_))
		raw match {
			case Parsed.Success(value, successIndex) =>
				println(value)
			case f @ Parsed.Failure(failureString, index, extra) =>
				println(f.trace(true))
		}
	}


	test("parsing to world query") {
		val Some(query) = WorldQueryParser.parse("select toHit, hp  from TestAttackData, TestCreatureData where hp > 3")

		val results = query.run(view)
		println(results)
	}

	test("assertion support") {
		val Some(unfilteredQuery) = WorldQueryParser.parse("assert toHit > 0 from TestAttackData")
		// not all assertions should have passed since one of the entities does not match the predicate
		assert(!unfilteredQuery.run(view).assertionsPassed)

		val Some(filteredQuery) = WorldQueryParser.parse("assert toHit > 0 from TestAttackData, TestCreatureData WHERE hp > 0 AND toHit != None")
		assert(filteredQuery.run(view).assertionsPassed)
	}

	test("absence query") {
		val Some(entityCQuery) = WorldQueryParser.parse("assert hp < 7 from TestAttackData, TestCreatureData WHERE toHit == None")
		assert(entityCQuery.run(view).assertionsPassed)
	}

	test("id support") {
		WorldQuery.assert(s"hp > 5 FROM TestCreatureData WHERE id == $entityA")
	}

	test("from bounded glob support") {
		val res = WorldQuery.run(s"select * FROM TestCreatureData WHERE id == $entityA")
		assert(res.entityResults(entityA)("hp").contains(10))
		assert(!res.entityResults(entityA).contains("toHit"))
	}

	test("unbounded glob support") {
		val res = WorldQuery.run(s"select * WHERE id == $entityA")
		assert(res.entityResults(entityA)("hp").contains(10))
		assert(res.entityResults(entityA)("toHit").contains(3))
	}

	test("context glob support") {
		val res = WorldQuery.run(s"select TestCreatureData.* WHERE id == $entityA")
		assert(res.entityResults(entityA)("hp").contains(10))
		assert(!res.entityResults(entityA).contains("toHit"))
	}
}
