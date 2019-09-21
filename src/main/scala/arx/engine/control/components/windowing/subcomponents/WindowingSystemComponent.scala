package arx.engine.control.components.windowing.subcomponents

import arx.core.TDependable
import arx.core.units.UnitOfTime
import arx.engine.control.components.windowing.{Widget, WindowingSystem}

abstract class WindowingSystemComponent extends TDependable {
	def updateWidget(windowingSystem: WindowingSystem, widget : Widget)
	def drawWidget(windowingSystem: WindowingSystem, widget : Widget)
}
