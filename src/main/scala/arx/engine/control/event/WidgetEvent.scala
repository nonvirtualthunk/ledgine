package arx.engine.control.event

import arx.engine.control.components.windowing.Widget

trait WidgetEvent extends ControlEvent {

}


case class WidgetDestroyedEvent(widget : Widget) extends ControlEvent