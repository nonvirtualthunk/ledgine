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
	private val belowPattern = "([0-9]+) below ([a-zA-Z0-9]+)".r
	private val rightLeftPattern = "(?i)([0-9]+) (right|left) of ([a-zA-Z0-9]+)".r
	def parse(s : String, siblings : List[Widget]) : Option[PositionExpression] = {
		Option(s.toLowerCase() match {
			case "flow" => Flow
			case "centered" | "center" => Centered
			case matchPattern(matchTarget) =>
				throw new UnsupportedOperationException("matching other widgets not yet supported")
			case proportionPattern(pcnt) =>
				Proportional(pcnt.toFloat / 100.0f)
			case orientedConstantPattern(amount, from) =>
				Constant(amount.toFloat.toInt, WindowingOrientation.fromString(from, TopLeft))
			case simpleConstantPattern(amount) =>
				Constant(amount.toFloat.toInt)
			case pxConstantPattern(amount) =>
				Constant(amount.toFloat.toInt)
			case belowPattern(amount, target) =>
				siblings.find(w => w.identifier.map(_.toLowerCase()).contains(target.toLowerCase()) || w.configIdentifier.map(_.toLowerCase()).contains(target.toLowerCase)) match {
					case Some(w) => Relative(w, amount.toFloat.toInt, Cardinals.Bottom)
					case None =>
						Noto.error(s"Relative position with no matching reference point $target")
						null
				}
			case rightLeftPattern(amount, rightLeft, target) =>
				siblings.find(w => w.identifier.map(_.toLowerCase()).contains(target.toLowerCase()) || w.configIdentifier.map(_.toLowerCase()).contains(target.toLowerCase)) match {
					case Some(w) =>
						if (rightLeft.toLowerCase() == "right") {
							Relative(w, amount.toFloat.toInt, Cardinals.Right)
						} else {
							Relative(w, amount.toFloat.toInt, Cardinals.Left)
						}
					case None =>
						Noto.error(s"Relative position with no matching reference point $target")
						null
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
		override def dependsOn(w : Widget) = Nil //w.widgetData.children
	}
	def MatchParent = Proportional(1.0f)
	case class ExpandTo(sibling : Widget) extends DimensionExpression {
		override def dependsOn(w: Widget): List[Widget] = List(sibling)
	}

	private val proportionPattern = "([0-9]+)%".r
	private val relativePattern = "rel\\((-?[0-9]+)\\)".r
	private val constantPattern = "([0-9]+)".r
	private val expandToPattern = "(?i)expand to ([a-zZ-Z0-9]+)".r
	def parse(s : String, siblings : List[Widget]) : Option[DimensionExpression] = {
		Option(s.toLowerCase().stripWhitespace match {
			case proportionPattern(pcnt) => Proportional(pcnt.toFloat / 100.0f)
			case relativePattern(delta) => Relative(delta.toFloat.toInt)
			case "expandtoparent" => ExpandToParent
			case "intrinsic" => Intrinsic
			case "wrapcontent" => WrapContent
			case constantPattern(amount) => Constant(amount.toFloat.toInt)
			case expandToPattern(target) => siblings.find(s => s.hasIdentifier(target)) match {
				case Some(sibling) => ExpandTo(sibling)
				case None =>
					Noto.warn(s"could not find target to expand to : $target")
					null
			}
			case _ =>
				Noto.warn(s"unsupported dimension expression : $s")
				null
		})
	}
}
