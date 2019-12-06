package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.introspection.ReflectionAssistant
import arx.core.vec.{Cardinal, Cardinals}
import arx.engine.control.components.windowing.Widget
import arx.engine.graphics.components.windowing.WindowingRenderer
import arx.graphics.Axis



trait PositionExpression {
	def dependsOn(w : Widget, axis : Axis) : List[(Widget, Axis)] = Nil
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
	case class Constant(value : Int, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression {
		override def dependsOn(widget : Widget, axis : Axis) = axis match {
			case Axis.X if relativeTo != TopLeft && relativeTo != BottomLeft => List(widget.parent -> Axis.X)
			case Axis.Y if relativeTo != BottomLeft && relativeTo != BottomRight => List(widget.parent -> Axis.Y)
			case _ => Nil
		}
	}
	case class Proportional(proportion : Float, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression {
		override def dependsOn(w: Widget, axis : Axis) = List(w.parent -> axis)
	}
	case object Centered extends PositionExpression {
		override def dependsOn(w: Widget, axis : Axis) = List(w.parent -> axis)
	}
	case class Relative(relativeTo: Widget, offset : Int, direction : Cardinal = Cardinals.Right) extends PositionExpression {
		override def dependsOn(widget : Widget, axis : Axis) = List(relativeTo -> axis)
	}
	case class Match(matchTo: Widget) extends PositionExpression {
		override def dependsOn(widget : Widget, axis : Axis) = List(matchTo -> axis)
	}
	case object Flow extends PositionExpression
	case class Absolute(value : Int, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression


	private val matchPattern = "match\\((.*)\\)".r
	private val proportionPattern = "([0-9]+)%".r
	private val orientedConstantPattern = "([0-9]+) from (.*)".r
	private val simpleConstantPattern = "([0-9]+)$".r
	private val pxConstantPattern = "([0-9]+)px$".r
	private val belowPattern = "([0-9]+) below ([a-zA-Z0-9]+)".r
	private val abovePattern = "([0-9]+) above ([a-zA-Z0-9]+)".r
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
			case abovePattern(amount, target) =>
				siblings.find(w => w.identifier.map(_.toLowerCase()).contains(target.toLowerCase()) || w.configIdentifier.map(_.toLowerCase()).contains(target.toLowerCase)) match {
					case Some(w) => Relative(w, amount.toFloat.toInt, Cardinals.Top)
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
	def dependsOn(w : Widget, axis : Axis, renderers : List[WindowingRenderer]) : List[(Widget, Axis)] = Nil
}

object DimensionExpression {
	case class Constant(value : Int) extends DimensionExpression
	case class Proportional(proportion : Float) extends DimensionExpression {
		override def dependsOn(w: Widget, axis: Axis, renderers : List[WindowingRenderer]): List[(Widget, Axis)] = List(w.parent -> axis)
	}
	case class Relative(delta : Int) extends DimensionExpression {
		override def dependsOn(w: Widget, axis: Axis, renderers : List[WindowingRenderer]): List[(Widget, Axis)] = List(w.parent -> axis)
	}
	case object ExpandToParent extends DimensionExpression {
		override def dependsOn(w: Widget, axis: Axis, renderers : List[WindowingRenderer]): List[(Widget, Axis)] = List(w.parent -> axis)
	}
	case object Intrinsic extends DimensionExpression {
		override def dependsOn(w: Widget, axis: Axis, renderers : List[WindowingRenderer]): List[(Widget, Axis)] = renderers.flatMap(r => r.intrinsicDependencies(w, axis))
	}
	case object WrapContent extends DimensionExpression {
		override def dependsOn(w : Widget, axis : Axis, renderers : List[WindowingRenderer]) = w.widgetData.children.map(c => c -> axis)
	}
	def MatchParent = Proportional(1.0f)
	case class ExpandTo(sibling : Widget) extends DimensionExpression {
		override def dependsOn(w: Widget, axis : Axis, renderers : List[WindowingRenderer]) = List(sibling -> axis)
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
