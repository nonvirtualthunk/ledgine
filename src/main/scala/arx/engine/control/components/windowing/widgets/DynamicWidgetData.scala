package arx.engine.control.components.windowing.widgets

import arx.application.Noto
import arx.core.geometry.{Horizontal, Orientation, Vertical}
import arx.core.macros.GenerateCompanion
import arx.core.representation.ConfigValue
import arx.core.vec.Cardinals
import arx.engine.control.components.windowing.{SimpleWidget, Widget, WidgetInstance, WidgetType}
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.control.event.{MouseButton, MouseReleaseEvent, UIEvent}

@GenerateCompanion
class DynamicWidgetData extends TWidgetAuxData {
	var dynWidgetFunctions : DynamicWidgetFunctions = new DynamicWidgetFunctions {
		override def computeChildrenData(dynWidget: Widget): List[Any] = Nil
		override def createChildFromData(dynWidget: Widget, data: Any): Widget = dynWidget.createChild(SimpleWidget).widget
		override def arrangeChildren(dynWidget: Widget, childWidgets: List[Widget]): Unit = {}
	}
	var lastChildrenData : List[Any] = Nil
	var lastChildren : List[Widget] = Nil
	var forceRecomputation : Boolean = false


	override def autoLoadSimpleValuesFromConfig: Boolean = false

	override def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {
		if (configValue.hasField("listItemArchetype")) {
			val archRef = configValue.field("listItemArchetype").str
			val bindingStr = configValue.field("listItemBinding").str
			val orientation = configValue.fieldOpt("orientation").flatMap(o => Orientation.fromStringOrWarn(o.str)).getOrElse(Vertical)
			val spacing = configValue.fieldOpt("listSpacing").flatMap(o => ListSpacing.fromStringOrWarn(o.str)).getOrElse(ListSpacing.Sequential)

			bindingStr match {
				case DynamicWidgetData.bindingParser(from, to) =>
					widget[DynamicWidgetData].dynWidgetFunctions = new ListWidgetDynamicFunctions(archRef, from, to, orientation, spacing)
				case _ => Noto.warn(s"invalid list widget binding clause: $bindingStr")
			}
		}
	}
}

object DynamicWidgetData {
	val bindingParser = "([a-zA-Z]+[a-zA-Z0-9]*)\\s*?->\\s*?([a-zA-Z]+[a-zA-Z0-9]*)".r
}

trait DynamicWidgetFunctions {
	def computeChildrenData(dynWidget : Widget) : List[Any]
	def createChildFromData(dynWidget: Widget, data : Any) : Widget
	def arrangeChildren(dynWidget : Widget, childWidgets : List[Widget])
}

class ListWidgetDynamicFunctions(archRef : String, bindingFrom : String, bindingTo : String, orientation : Orientation, spacing : ListSpacing) extends DynamicWidgetFunctions {
	import arx.Prelude._

	override def computeChildrenData(dynWidget: Widget): List[Any] = {
		dynWidget.resolveBinding(bindingFrom).map {
			case s : Seq[_] => s.zipWithIndex.map(_._2).toList
			case _ => Nil
		}.getOrElse(Nil)
	}

	override def createChildFromData(dynWidget: Widget, data: Any): Widget = {
		data match {
			case index : Int =>
				val child = dynWidget.createChild(archRef)
				child.notConfigManaged = true
				child.bind(bindingTo, () => dynWidget.resolveBinding(bindingFrom) match {
					case Some(boundSeq) =>
						boundSeq match {
							case s : Seq[_] if s.size > index => Some(s(index))
							case s : Seq[_] if s.isEmpty => None
							case other =>
								Noto.warn(s"list binding is to non sequence, that won't work: $other")
								None
						}
					case _ => None
				})
				child.onEvent {
					case MouseReleaseEvent(MouseButton.Left, _,_) =>
						dynWidget.handleEvent(ListItemSelected(dynWidget.effectiveIdentifier, index, child.resolveBinding(bindingTo)))
				}
				child
			case _ => ???
		}
	}

	override def arrangeChildren(dynWidget: Widget, childWidgets: List[Widget]): Unit = {
		val gapSize = dynWidget.dataOpt[ListWidgetData].map(_.listItemGapSize).getOrElse(2)
		spacing match {
			case ListSpacing.Sequential =>
				childWidgets.sliding2.foreach {
					case (a,b) => orientation match {
						case Vertical => b.widgetData.y = PositionExpression.Relative(a, gapSize, Cardinals.Bottom)
						case Horizontal => b.widgetData.x = PositionExpression.Relative(a, gapSize, Cardinals.Right)
					}
				}
			case ListSpacing.Even =>
				childWidgets.size match {
					case 0 => // do nothing
					case 1 => orientation match {
						case Horizontal => childWidgets.head.x = PositionExpression.Centered
						case Vertical => childWidgets.head.y = PositionExpression.Centered
					}
					case _ =>
						// TODO : deal with case in which naive placement puts the first/last outside of bounds
						val increment = 1.0f / childWidgets.size.toFloat

						childWidgets.zipWithIndex.foreach {
							case (child, index) =>
								val fract = (index.toFloat + 0.5f) * increment
								orientation match {
									case Horizontal => child.widgetData.x = PositionExpression.Proportional(fract, anchorTo=Center)
									case Vertical => child.widgetData.y = PositionExpression.Proportional(fract, anchorTo=Center)
								}
						}
				}

				// 0 1 2 3 4 5 6 7 8 9 0 1 2
				//       X X X   X X X
				// 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
				//       X X X   X X X   X X X

				// 0 1 2 3 4 5 6 7 8 9 0
				// -----   -   -----   -
				//   -     -     -     -

		}
	}
}

sealed trait ListSpacing
object ListSpacing {
	case object Sequential extends ListSpacing
	case object Even extends ListSpacing

	def fromStringOrWarn(str : String) : Option[ListSpacing] = {
		val ret = str.toLowerCase() match {
			case "sequential" => Some(Sequential)
			case "even" => Some(Even)
			case "default" => Some(Sequential)
			case _ => None
		}
		if (ret.isEmpty) {
			Noto.warn(s"Invalid list spacing string $str, valid options are ${Seq(Sequential, Even)} or Default")
		}
		ret
	}
}

@GenerateCompanion
class ListWidgetData extends TWidgetAuxData {
	var listItemGapSize : Int = 0
}


class ListWidget(val widget : Widget) extends WidgetInstance {
}
object ListWidget extends WidgetType[ListWidget, ListWidgetData] {
	override def initializeWidget(widget: Widget): ListWidget = {
		widget.attachData[DynamicWidgetData]
		widget.attachData[ListWidgetData]
		new ListWidget(widget)
	}
}


class DynamicWidget(val widget : Widget) extends WidgetInstance {
}
object DynamicWidget extends WidgetType[DynamicWidget, DynamicWidgetData] {
	override def initializeWidget(widget: Widget): DynamicWidget = {
		widget.attachData[DynamicWidgetData]
		new DynamicWidget(widget)
	}
}

case class ListItemSelected(listId : String, index : Int, data : Option[Any]) extends UIEvent