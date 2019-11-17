package arx.graphics

import arx.core.vec.Vec2i

import scala.language.implicitConversions

class Axis(val axis : Int) extends AnyVal {
	def v2(l : Int) = {
		val v2i = Vec2i(0,0)
		v2i(axis) = l
		v2i
	}

	override def toString: String = axis match {
		case 0 => "X"
		case 1 => "Y"
		case 2 => "Z"
		case _ => "Unknown Axis"
	}
}

object Axis {
	val X = new Axis(0)
	val Y = new Axis(1)
	val Z = new Axis(2)

	val XY = List(X, Y)
	val XYZ = List(X, Y, Z)

	implicit def toInt(axis : Axis) : Int = axis.axis
}
