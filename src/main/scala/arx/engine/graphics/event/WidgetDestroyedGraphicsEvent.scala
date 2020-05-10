package arx.engine.graphics.event

import arx.engine.control.components.windowing.Widget

case class WidgetDestroyedGraphicsEvent(widget : Widget) extends GraphicsEvent {

}
