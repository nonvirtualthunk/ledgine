package arx.engine.control.components.windowing

import arx.engine.control.components.windowing.WidgetTest.Foo
import arx.engine.data.Moddable
import arx.engine.world.World
import org.scalatest.FunSuite

class WidgetTest extends FunSuite {
	val world = new World
	val windowingSystem = new WindowingSystem(world, _ => {})

	test("binding resolution - basic case") {
		val w = windowingSystem.createWidget()
		w.bind("a", 1)

		assert(w.resolveBinding("a").contains(1))
	}

	test("binding resolution - non-sub dots") {
		val w = windowingSystem.createWidget()
		w.bind("a.b.c", 1)

		assert(w.resolveBinding("a.b.c").contains(1))
	}

	test("binding resolution - parent reference") {
		val w = windowingSystem.createWidget()
		val c = windowingSystem.createWidget()
		c.parent = w

		w.bind("a", () => 3)
		assert(c.resolveBinding("a").contains(3))
	}

	test("binding resolution - map sub-bindings") {
		val w = windowingSystem.createWidget()
		val c = windowingSystem.createWidget()
		c.parent = w

		val amap = Map("b" -> Map("c" -> 9, "d" -> Moddable(() => 10)), "f" -> (() => Map("v" -> 1)))
		w.bind("a", amap)
		assert(c.resolveBinding("a.b.c").contains(9))
		assert(c.resolveBinding("a.b.d").contains(10))
		assert(c.resolveBinding("a.f.v").contains(1))
		assert(c.resolveBinding("a").contains(amap))
	}

	test("binding resolution - object sub-bindings") {
		val w = windowingSystem.createWidget()
		val c = windowingSystem.createWidget()
		c.parent = w

		w.bind("a", Foo(3))
		assert(c.resolveBinding("a.value").contains(3))
	}
}

object WidgetTest {
	case class Foo(value : Int)
}