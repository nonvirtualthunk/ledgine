package arx.engine.control.components.windowing
import arx.core.introspection.Field
import arx.core.introspection.Clazz
object Companions {
import arx.engine.control.components.windowing.WidgetData
object WidgetData extends Clazz[WidgetData]("WidgetData", classOf[WidgetData]){
	val Sentinel = new WidgetData
	override def instantiate = new WidgetData
	val widget = Field.fromValue(Sentinel.widget).createField[WidgetData]("widget",f => f.widget, (f,widget) => f.widget = widget, WidgetData) 
	fields += "widget" -> widget
	val parent = Field.fromValue(Sentinel.parent).createField[WidgetData]("parent",f => f.parent, (f,parent) => f.parent = parent, WidgetData) 
	fields += "parent" -> parent
	val children = Field.fromValue(Sentinel.children).createField[WidgetData]("children",f => f.children, (f,children) => f.children = children, WidgetData) 
	fields += "children" -> children
	val identifier = Field.fromValue(Sentinel.identifier).createField[WidgetData]("identifier",f => f.identifier, (f,identifier) => f.identifier = identifier, WidgetData) 
	fields += "identifier" -> identifier
	val bindings = Field.fromValue(Sentinel.bindings).createField[WidgetData]("bindings",f => f.bindings, (f,bindings) => f.bindings = bindings, WidgetData) 
	fields += "bindings" -> bindings
	val position = Field.fromValue(Sentinel.position).createField[WidgetData]("position",f => f.position, (f,position) => f.position = position, WidgetData) 
	fields += "position" -> position
	val dimensions = Field.fromValue(Sentinel.dimensions).createField[WidgetData]("dimensions",f => f.dimensions, (f,dimensions) => f.dimensions = dimensions, WidgetData) 
	fields += "dimensions" -> dimensions
	val showing = Field.fromValue(Sentinel.showing).createField[WidgetData]("showing",f => f.showing, (f,showing) => f.showing = showing, WidgetData) 
	fields += "showing" -> showing
	val acceptsFocus = Field.fromValue(Sentinel.acceptsFocus).createField[WidgetData]("acceptsFocus",f => f.acceptsFocus, (f,acceptsFocus) => f.acceptsFocus = acceptsFocus, WidgetData) 
	fields += "acceptsFocus" -> acceptsFocus
	val hasFocus = Field.fromValue(Sentinel.hasFocus).createField[WidgetData]("hasFocus",f => f.hasFocus, (f,hasFocus) => f.hasFocus = hasFocus, WidgetData) 
	fields += "hasFocus" -> hasFocus
	val markedModified = Field.fromValue(Sentinel.markedModified).createField[WidgetData]("markedModified",f => f.markedModified, (f,markedModified) => f.markedModified = markedModified, WidgetData) 
	fields += "markedModified" -> markedModified
	val modificationCriteria = Field.fromValue(Sentinel.modificationCriteria).createField[WidgetData]("modificationCriteria",f => f.modificationCriteria, (f,modificationCriteria) => f.modificationCriteria = modificationCriteria, WidgetData) 
	fields += "modificationCriteria" -> modificationCriteria
	val modificationWatchers = Field.fromValue(Sentinel.modificationWatchers).createField[WidgetData]("modificationWatchers",f => f.modificationWatchers, (f,modificationWatchers) => f.modificationWatchers = modificationWatchers, WidgetData) 
	fields += "modificationWatchers" -> modificationWatchers
	val eventListeners = Field.fromValue(Sentinel.eventListeners).createField[WidgetData]("eventListeners",f => f.eventListeners, (f,eventListeners) => f.eventListeners = eventListeners, WidgetData) 
	fields += "eventListeners" -> eventListeners
	val successors = Field.fromValue(Sentinel.successors).createField[WidgetData]("successors",f => f.successors, (f,successors) => f.successors = successors, WidgetData) 
	fields += "successors" -> successors

	def apply(f : WidgetData => Unit) : WidgetData = { val v = new WidgetData; f(v); v }
					 
}
import arx.engine.control.components.windowing.WidgetPrototypeData
object WidgetPrototypeData extends Clazz[WidgetPrototypeData]("WidgetPrototypeData", classOf[WidgetPrototypeData]){
	val Sentinel = new WidgetPrototypeData
	override def instantiate = new WidgetPrototypeData
	val prototype = Field.fromValue(Sentinel.prototype).createField[WidgetPrototypeData]("prototype",f => f.prototype, (f,prototype) => f.prototype = prototype, WidgetPrototypeData) 
	fields += "prototype" -> prototype

	def apply(f : WidgetPrototypeData => Unit) : WidgetPrototypeData = { val v = new WidgetPrototypeData; f(v); v }
					 
}
}
