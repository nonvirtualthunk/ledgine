package arx.engine.control.components.windowing.subcomponents
import arx.core.metrics.Metrics
import arx.engine.control.components.windowing.widgets.DynamicWidgetData
import arx.engine.control.components.windowing.{Widget, WindowingSystem}

object DynamicWidgetComponent extends WindowingSystemComponent {
	val recomputingDynamicWidgetMeter = Metrics.meter("DynamicWidgetComponent.recreate-children")
	override def updateWidget(windowingSystem: WindowingSystem, widget: Widget): Unit = {
		for (dyn <- widget.dataOpt[DynamicWidgetData]) {
			val newChildrenData = dyn.dynWidgetFunctions.computeChildrenData(widget)
			if (newChildrenData != dyn.lastChildrenData) {
				dyn.lastChildrenData = newChildrenData

				recomputingDynamicWidgetMeter.mark()
				// TODO: This could be more efficient
				println("remaking children")
				dyn.lastChildren.foreach(_.destroy())
				val newChildren = newChildrenData.map(d => dyn.dynWidgetFunctions.createChildFromData(widget, d))
				dyn.dynWidgetFunctions.arrangeChildren(widget, newChildren)
				dyn.lastChildren = newChildren
			}
		}
	}

	override def drawWidget(windowingSystem: WindowingSystem, widget: Widget): Unit = {}
}
