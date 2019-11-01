package arx.engine.control.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.control.components.windowing.Widget
import arx.engine.data.{TMutableAuxData, TWorldAuxData}

import scala.collection.immutable.Stack


class WindowingControlData extends TControlData with TMutableAuxData with TWorldAuxData {
	var desktop : Widget = _
	var focusedWidget : Option[Widget] = None
	var lastWidgetUnderMouse : Option[Widget] = None
	var currentPressedWidget : Option[Widget] = None
	var draggingWidget : Option[Widget] = None
	var modalWidgetStack = Stack[ModalWidget]()
}

case class ModalWidget (widget : Widget, closeOnClickElsewhere : Boolean)