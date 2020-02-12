package arx.core.geometry

import arx.application.Noto
import arx.graphics.Axis

sealed trait Orientation {
	def axis : Axis
}
case object Horizontal extends Orientation {
	override def axis = Axis.X
}
case object Vertical extends Orientation {
	override def axis = Axis.Y
}
object Orientation {
	def fromString(str : String) : Option[Orientation] = {
		str.toLowerCase() match {
			case "horizontal" => Some(Horizontal)
			case "vertical" => Some(Vertical)
			case _ => None
		}
	}

	def fromStringOrWarn(str : String) : Option[Orientation] = {
		val ret = fromString(str)
		if (ret.isEmpty) {
			Noto.warn(s"Invalid orientation string: $str")
		}
		ret
	}
}