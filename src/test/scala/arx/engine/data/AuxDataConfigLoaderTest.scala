package arx.engine.data

import arx.core.representation.Hocon
import arx.engine.data.AuxDataConfigLoaderTest.{SubObj, TestAuxData}
import arx.resource.ResourceManager
import org.scalatest.FunSuite
import arx.Prelude._
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec3f, ReadVec3i, ReadVec4f, ReadVec4i, Vec2f, Vec2i, Vec3f, Vec3i, Vec4f, Vec4i}

class AuxDataConfigLoaderTest extends FunSuite {

	test("testLoadSimpleValuesFromConfig happy path") {

		val fullConf = Hocon.parse(
			"""
			  |{
			  |	i : 9
			  |   modi : 6
			  | 	f : 23.0
			  |  	modf : 24.0
			  |  	str : "this is effective"
			  |   modStr : "new value"
			  |   b : true
			  |   modB : true
			  |   listI : [1,2,3]
			  |   sub : {
			  |   	x : 9
			  |    	y : "Success"
			  |   }
			  |   v2 : [1.0,1.0]
			  |   v3 : [1.0,1.0,1.0]
			  |   v4 : [1.0,1.0,1.0,1.0]
			  |   v2i : [1,1]
			  |   v3i : [1,1,1]
			  |   v4i : [1,1,1,1]
			  |   modv2 : [1.0,1.0]
			  |
			  |   notDealtWith : "ignored"
			  |}
			  |""".stripMargin)

		val AD = new TestAuxData

		AuxDataConfigLoader.loadSimpleValuesFromConfig(AD, fullConf)

		assert(AD == TestAuxData(
			i = 9,
			f = 23.0f,
			modi = Moddable(6),
			modf = Moddable(24.0f),
			str = "this is effective",
			notDealtWith = new StringBuilder,
			modStr = Moddable("new value"),
			b = true,
			modB = Moddable(true),
			listI = List(1,2,3),
			sub = SubObj(9, "Success"),
			v2 = Vec2f(1.0f,1.0f),
			v3 = Vec3f(1.0f,1.0f,1.0f),
			v4 = Vec4f(1.0f,1.0f,1.0f,1.0f),
			v2i = Vec2i(1,1),
			v3i = Vec3i(1,1,1),
			v4i = Vec4i(1,1,1,1),
			modv2 = Moddable(Vec2f(1.0f,1.0f))
		))
	}

	test("testLoadSimpleValuesFromConfig empty path") {

		val fullConf = Hocon.parse("{}")

		val AD = new TestAuxData

		AuxDataConfigLoader.loadSimpleValuesFromConfig(AD, fullConf)

		assert(AD == TestAuxData())
	}



}


object AuxDataConfigLoaderTest {
	protected[data] case class SubObj(
		var x : Int = 0,
		var y : String = "A"
	)

	protected[data] case class TestAuxData(
		var i : Int = 3,
		var modi : Moddable[Int] = Moddable(3),
		var modf : Moddable[Float] = Moddable(1.0f),
		var f : Float = 19.0f,
		var str : String = "test",
		var modStr : Moddable[String] = Moddable("default"),
		var b : Boolean = false,
		var modB : Moddable[Boolean] = Moddable(false),
		var listI : List[Int] = List(4,5,6),
		var sub : SubObj = SubObj(),
		var v2 : ReadVec2f = Vec2f.Zero,
		var v3 : ReadVec3f = Vec3f.Zero,
		var v4 : ReadVec4f = Vec4f.Zero,
		var v2i : ReadVec2i = Vec2i.Zero,
		var v3i : ReadVec3i = Vec3i.Zero,
		var v4i : ReadVec4i = Vec4i.Zero,
		var modv2 : Moddable[ReadVec2f] = Moddable(Vec2f.Zero),
		var notDealtWith : StringBuilder = new StringBuilder) extends TAuxData {
	}
}