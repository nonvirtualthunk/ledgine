package arx.engine.control.components.windowing.widgets
import arx.core.introspection.Field
import arx.core.introspection.Clazz
object Companions {
import arx.engine.control.components.windowing.widgets.TextDisplay
object TextDisplay extends Clazz[TextDisplay]("TextDisplay", classOf[TextDisplay]){
	val Sentinel = new TextDisplay
	override def instantiate = new TextDisplay
	val text = Field.fromValue(Sentinel.text).createField[TextDisplay]("text",f => f.text, (f,text) => f.text = text, TextDisplay) 
	fields += "text" -> text
	val fontScale = Field.fromValue(Sentinel.fontScale).createField[TextDisplay]("fontScale",f => f.fontScale, (f,fontScale) => f.fontScale = fontScale, TextDisplay) 
	fields += "fontScale" -> fontScale
	val fontColor = Field.fromValue(Sentinel.fontColor).createField[TextDisplay]("fontColor",f => f.fontColor, (f,fontColor) => f.fontColor = fontColor, TextDisplay) 
	fields += "fontColor" -> fontColor
	val font = Field.fromValue(Sentinel.font).createField[TextDisplay]("font",f => f.font, (f,font) => f.font = font, TextDisplay) 
	fields += "font" -> font
	val textAlignment = Field.fromValue(Sentinel.textAlignment).createField[TextDisplay]("textAlignment",f => f.textAlignment, (f,textAlignment) => f.textAlignment = textAlignment, TextDisplay) 
	fields += "textAlignment" -> textAlignment
	val orientFromTop = Field.fromValue(Sentinel.orientFromTop).createField[TextDisplay]("orientFromTop",f => f.orientFromTop, (f,orientFromTop) => f.orientFromTop = orientFromTop, TextDisplay) 
	fields += "orientFromTop" -> orientFromTop

	def apply(f : TextDisplay => Unit) : TextDisplay = { val v = new TextDisplay; f(v); v }
					 
}
}
