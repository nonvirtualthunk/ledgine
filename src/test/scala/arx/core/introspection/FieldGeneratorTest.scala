package arx.core.introspection

import org.scalatest.FunSuite

class FieldGeneratorTest extends FunSuite {
	test("generate test class fields") {
		FieldGenerator.generate("arx", testClasses = true)
	}
}
