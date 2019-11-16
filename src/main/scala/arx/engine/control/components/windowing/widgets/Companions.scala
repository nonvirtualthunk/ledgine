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
					 
	def copyInto(from : TextDisplay, to : TextDisplay) {
		to.text = from.text
		to.fontScale = from.fontScale
		to.fontColor = from.fontColor
		to.font = from.font
		to.textAlignment = from.textAlignment
		to.orientFromTop = from.orientFromTop
	}
}
import arx.engine.control.components.windowing.widgets.ListWidgetData
object ListWidgetData extends Clazz[ListWidgetData]("ListWidgetData", classOf[ListWidgetData]){
	val Sentinel = new ListWidgetData
	override def instantiate = new ListWidgetData
	val listItemGapSize = Field.fromValue(Sentinel.listItemGapSize).createField[ListWidgetData]("listItemGapSize",f => f.listItemGapSize, (f,listItemGapSize) => f.listItemGapSize = listItemGapSize, ListWidgetData) 
	fields += "listItemGapSize" -> listItemGapSize

	def apply(f : ListWidgetData => Unit) : ListWidgetData = { val v = new ListWidgetData; f(v); v }
					 
	def copyInto(from : ListWidgetData, to : ListWidgetData) {
		to.listItemGapSize = from.listItemGapSize
	}
}
import arx.engine.control.components.windowing.widgets.DynamicWidgetData
object DynamicWidgetData extends Clazz[DynamicWidgetData]("DynamicWidgetData", classOf[DynamicWidgetData]){
	val Sentinel = new DynamicWidgetData
	override def instantiate = new DynamicWidgetData
	val dynWidgetFunctions = Field.fromValue(Sentinel.dynWidgetFunctions).createField[DynamicWidgetData]("dynWidgetFunctions",f => f.dynWidgetFunctions, (f,dynWidgetFunctions) => f.dynWidgetFunctions = dynWidgetFunctions, DynamicWidgetData) 
	fields += "dynWidgetFunctions" -> dynWidgetFunctions
	val lastChildrenData = Field.fromValue(Sentinel.lastChildrenData).createField[DynamicWidgetData]("lastChildrenData",f => f.lastChildrenData, (f,lastChildrenData) => f.lastChildrenData = lastChildrenData, DynamicWidgetData) 
	fields += "lastChildrenData" -> lastChildrenData
	val lastChildren = Field.fromValue(Sentinel.lastChildren).createField[DynamicWidgetData]("lastChildren",f => f.lastChildren, (f,lastChildren) => f.lastChildren = lastChildren, DynamicWidgetData) 
	fields += "lastChildren" -> lastChildren

	def apply(f : DynamicWidgetData => Unit) : DynamicWidgetData = { val v = new DynamicWidgetData; f(v); v }
					 
	def copyInto(from : DynamicWidgetData, to : DynamicWidgetData) {
		to.dynWidgetFunctions = from.dynWidgetFunctions
		to.lastChildrenData = from.lastChildrenData
		to.lastChildren = from.lastChildren
	}
}
}
