package arx.engine.control.components.windowing.widgets

import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.core.vec.Cardinals
import arx.engine.control.components.windowing.{SimpleWidget, Widget, WidgetInstance, WidgetType}
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData

class DynamicWidgetData extends TWidgetAuxData {
	var dynWidgetFunctions : DynamicWidgetFunctions = new DynamicWidgetFunctions {
		override def computeChildrenData(dynWidget: Widget): List[Any] = Nil
		override def createChildFromData(dynWidget: Widget, data: Any): Widget = dynWidget.createChild(SimpleWidget).widget
		override def arrangeChildren(dynWidget: Widget, childWidgets: List[Widget]): Unit = {}
	}
	var lastChildrenData : List[Any] = Nil
	var lastChildren : List[Widget] = Nil

	override def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {
		if (configValue.hasField("listItemArchetype")) {
			val archRef = configValue.field("listItemArchetype").str
			val bindingStr = configValue.field("listItemBinding").str
			bindingStr match {
				case DynamicWidgetData.bindingParser(from, to) =>
					widget[DynamicWidgetData].dynWidgetFunctions = new ListWidgetDynamicFunctions(archRef, from, to)
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

class ListWidgetDynamicFunctions(archRef : String, bindingFrom : String, bindingTo : String) extends DynamicWidgetFunctions {
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
				child.bind(bindingTo, () => dynWidget.resolveBinding(bindingFrom).flatMap {
					case s : Seq[_] if s.size > index => Some(s(index))
					case other =>
						Noto.warn(s"list binding is to non sequence, that won't work: $other")
						None
				})
				child
			case _ => ???
		}
	}

	override def arrangeChildren(dynWidget: Widget, childWidgets: List[Widget]): Unit = {
		val gapSize = dynWidget.dataOpt[ListWidgetData].map(_.listItemGapSize).getOrElse(2)
		childWidgets.sliding2.foreach {
			case (a,b) => b.widgetData.y = PositionExpression.Relative(a, gapSize, Cardinals.Bottom)
		}
	}
}

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