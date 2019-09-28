package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.Cardinals
import arx.engine.control.components.windowing.Widget



trait PositionExpression {
	def dependsOn : List[Widget] = Nil
}

sealed class WindowingOrientation {}
case object TopLeft extends WindowingOrientation
case object BottomRight extends WindowingOrientation
case object TopRight extends WindowingOrientation
case object BottomLeft extends WindowingOrientation
object WindowingOrientation {
	def fromString(str : String, default : WindowingOrientation) : WindowingOrientation = {
		str.toLowerCase().replace(" ","") match {
			case "topleft" => TopLeft
			case "bottomright" => BottomRight
			case "bottomleft" => BottomLeft
			case "topright" => TopRight
			case "left" => TopLeft
			case "right" => TopRight
			case "top" => TopLeft
			case "bottom" => BottomLeft
			case _ => default
		}
	}
}

object PositionExpression {
	case class Constant(value : Int, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression
	case class Proportional(proportion : Float, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression
	case object Centered extends PositionExpression
	case class Relative(relativeTo: Widget, offset : Int, direction : Int = Cardinals.Right) extends PositionExpression {
		override def dependsOn = List(relativeTo)
	}
	case class Match(matchTo: Widget) extends PositionExpression {
		override def dependsOn: List[Widget] = List(matchTo)
	}
	case object Flow extends PositionExpression


	private val matchPattern = "match\\((.*)\\)".r
	private val proportionPattern = "([0-9]+)%".r
	private val orientedConstantPattern = "([0-9]+) from (.*)".r
	private val simpleConstantPattern = "([0-9]+)$".r
	private val pxConstantPattern = "([0-9]+)px$".r
	def parse(s : String) : Option[PositionExpression] = {
		Option(s.toLowerCase() match {
			case "flow" => Flow
			case "centered" | "center" => Centered
			case matchPattern(matchTarget) => {
				throw new UnsupportedOperationException("matching other widgets not yet supported")
			}
			case proportionPattern(pcnt) => {
				Proportional(pcnt.toFloat / 100.0f)
			}
			case orientedConstantPattern(amount, from) => {
				Constant(amount.toFloat.toInt, WindowingOrientation.fromString(from, TopLeft))
			}
			case simpleConstantPattern(amount) => {
				Constant(amount.toFloat.toInt)
			}
			case pxConstantPattern(amount) => {
				Constant(amount.toFloat.toInt)
			}
			case _ =>
				Noto.error(s"unsupported position expression: $s")
				null
		})
	}
}


sealed trait DimensionExpression {
	def dependsOn(w : Widget) : List[Widget] = Nil
}

object DimensionExpression {
	case class Constant(value : Int) extends DimensionExpression
	case class Proportional(proportion : Float) extends DimensionExpression
	case class Relative(delta : Int) extends DimensionExpression
	case object ExpandToParent extends DimensionExpression
	case object Intrinsic extends DimensionExpression
	case object WrapContent extends DimensionExpression {
		override def dependsOn(w : Widget) = w.widgetData.children
	}
	def MatchParent = Proportional(1.0f)

	private val proportionPattern = "([0-9]+)%".r
	private val relativePattern = "rel\\(([0-9]+)\\)".r
	private val constantPattern = "([0-9]+)".r
	def parse(s : String) : Option[DimensionExpression] = {
		Option(s.toLowerCase() match {
			case proportionPattern(pcnt) => Proportional(pcnt.toFloat / 100.0f)
			case relativePattern(delta) => Relative(delta.toFloat.toInt)
			case "expandtoparent" => ExpandToParent
			case "intrinsic" => Intrinsic
			case "wrapcontent" => WrapContent
			case constantPattern(amount) => Constant(amount.toFloat.toInt)
			case _ => null
		})
	}
}
