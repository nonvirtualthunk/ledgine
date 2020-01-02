package arx.engine.control.components.windowing.widgets

import arx.engine.data.Moddable
import arx.core.vec.{ReadVec4f, Vec2T, Vec2i, Vec4f}
import arx.engine.control.components.windowing.{Widget, WidgetInstance, WidgetType}
import arx.engine.control.event.{CharEnteredEvent, KeyPressEvent, KeyReleaseEvent, TextInputChanged, TextInputEnter}
import arx.graphics.helpers.{Color, RichText, TextSection}
import arx.Prelude._
import arx.core.introspection.ReflectionAssistant
import arx.engine.control.components.windowing.events.{FocusGainedEvent, FocusLostEvent}
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import org.lwjgl.glfw.GLFW

class TextInputData extends TWidgetAuxData {
	var rawText = ""
}

case class TextInputWidget(widget : Widget) extends WidgetInstance {

}

object TextInputWidget extends WidgetType[TextInputWidget, TextInputData] {

	override def initializeWidget(widget: Widget): TextInputWidget = {
		widget.attachData[TextDisplay]
		widget.attachData[TextInputData]
		widget.acceptsFocus = true

		val tid = widget[TextInputData]

		widget[TextDisplay].text = Moddable(() => RichText(widget[TextInputData].rawText))
		widget.onEvent {
			case CharEnteredEvent(str) =>
				tid.rawText += str
				widget.handleEvent(TextInputChanged(tid.rawText))
			case KeyReleaseEvent(GLFW.GLFW_KEY_BACKSPACE, _) =>
				tid.rawText = tid.rawText.dropRight(1)
				widget.handleEvent(TextInputChanged(tid.rawText))
			case KeyReleaseEvent(GLFW.GLFW_KEY_ENTER, _) =>
				widget.handleEvent(TextInputEnter(tid.rawText))
			case FocusGainedEvent(focused) if focused == widget =>
				widget.markModified()
			case FocusLostEvent(focused) if focused == widget =>
				widget.markModified()
		}

		TextInputWidget(widget)
	}
}


//class TextInputWidget(parentis : Widget) extends Widget(parentis) {
//	var cursorPosition : Option[Int] = None
//	protected var _text = ""
//	def text = _text
//	var promptText = ""
//	var promptColor : Moddable[ReadVec4f] = Moddable(Vec4f(0.5f,0.5f,0.6f,1.0f))
//
//	var singleLine = true
//
//	drawing.backgroundImage = Some("ui/minimalistBorderWhite_ne.png")
//	drawing.interiorPadding = Vec2i(4,4)
//
//	val textDisplay = new TextDisplayWidget(this)
//	textDisplay.dimensions = Vec2T(DimensionExpression.Intrinsic, DimensionExpression.Intrinsic)
//	textDisplay.text = Moddable(() => richTextFromRaw(_text))
//
//	def richTextFromRaw(str : String) = RichText(str)
//
//	val promptTextDisplay = new TextDisplayWidget(this)
//	promptTextDisplay.dimensions = Vec2T(DimensionExpression.Intrinsic, DimensionExpression.Intrinsic)
//	promptTextDisplay.text = Moddable(() => RichText(TextSection(promptText, promptColor.resolve(), None) :: Nil))
//	promptTextDisplay.x = PositionExpression.Relative(textDisplay, 1)
//
//
//	def setText(newText : String, suppressEvent: Boolean = false): Unit = {
//		_text = newText
//		if (!suppressEvent) {
//			handleEvent(TextInputChanged(_text))
//		}
//	}
//
//	def setTextPrompt(newPrompt : String) : Unit = {
//		promptText = newPrompt
//	}
//
//	onEvent {
//		case CharEnteredEvent(str) =>
//			setText(text + str)
//		case KeyPressEvent(key, modifiers, _) =>
//			key pmatch {
//				case GLFW.GLFW_KEY_BACKSPACE =>
//					setText(text.dropRight(1))
//				case GLFW.GLFW_KEY_ENTER =>
//					if (singleLine) {
//						handleEvent(TextInputEnter(text))
//					} else {
//						setText(text + "\n")
//					}
//			}
//
//	}
//}