package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.Watcher
import arx.core.introspection.{ReflectionAssistant, TEagerSingleton}
import arx.core.macros.GenerateCompanion
import arx.core.representation.ConfigValue
import arx.core.traits.TSentinelable
import arx.core.vec.{Vec2T, Vec3T}
import arx.engine.control.components.windowing.widgets.DimensionExpression.Intrinsic
import arx.engine.control.components.windowing.widgets.PositionExpression.Flow
import arx.engine.control.components.windowing.widgets.data.{DragAndDropData, DrawingData, TWidgetAuxData}
import arx.engine.control.components.windowing.widgets.{DimensionExpression, ImageDisplayWidget, PositionExpression}
import arx.engine.control.data.WindowingControlData
import arx.engine.control.event.{Mouse, MouseButton, MousePressEvent, MouseReleaseEvent}
import arx.engine.data.Moddable
import arx.engine.entity.Entity
import arx.engine.event.TEventUser
import arx.engine.graphics.data.windowing.ImageDisplay

import scala.reflect.ClassTag


class Widget(val entity : Entity, val windowingSystem : WindowingSystem) extends TSentinelable {
	windowingSystem.displayWorld.attachData(entity).ofType((wd : WidgetData) => {
		wd.widget = this
		wd.successors ::= Moddable(() => if (wd.parent.notSentinel) { wd.parent.widgetData } else { TEventUser.DevNull })
		wd
	}).ofType((dd : DrawingData) => dd)

	val widgetData = windowingSystem.displayWorld.data[WidgetData](entity)
	val drawing = windowingSystem.displayWorld.data[DrawingData](entity)
	def dragAndDropRO = dataOpt[DragAndDropData].getOrElse(DragAndDropData.Default)

	override def isSentinel: Boolean = entity.isSentinel

	def close(): Unit = {
		destroy()
	}

	def isUnderPress = windowingSystem.displayWorld[WindowingControlData].lastWidgetUnderMouse
		.exists(w => w.selfAndAncestors.contains(this)) && Mouse.buttonDown(MouseButton.Left)

	def attachData[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.data[T](entity)
	def apply[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.data[T](entity)
	def data[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.data[T](entity)
	def dataOpt[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.dataOpt[T](entity)
	def hasData[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.view.hasData[T](entity)
	def allData = windowingSystem.displayWorld.allData(entity)

	def withWidgetData(f : WidgetData => Unit) : Unit = { f(this.data[WidgetData]) }
	def withDrawingData(f : DrawingData => Unit) : Unit = { f(this.data[DrawingData]) }

	def destroy(): Unit = {
		widgetData.onClose()
		windowingSystem.destroyWidget(this)
	}

	def createChild[U <: WidgetInstance, D <: TWidgetAuxData](ofType: WidgetConstructor[U]) : U = {
		val w = windowingSystem.createWidget()
		w.parent = this
		ofType.initializeWidget(w)
	}

	def createChild(dottedConfigPath : String) : Widget = {
		val sections = dottedConfigPath.split('.')
		if (sections.length == 2) {
			createChild(sections(0), sections(1))
		} else {
			Noto.error("createChild(dottedConfigPath) expects an identifier in the form of WidgetFile.WidgetKey")
			val w = windowingSystem.createWidget()
			w.parent = this
			w
		}
	}
	def createChild(resourcePath : String, key : String) : Widget = {
		val w = windowingSystem.createWidget(resourcePath, key)
		w.parent = this
		w
	}

	def childWithIdentifier(identifier : String) : Option[Widget] = {
		widgetData.children.find(c => c.identifier.contains(identifier) || c.configIdentifier.contains(identifier))
	}
	def descendantsWithIdentifier(identifier : String) : List[Widget] = {
		widgetData.children.filter(c => c.identifier.contains(identifier) || c.configIdentifier.contains(identifier)) :::
			widgetData.children.flatMap(c => c.descendantsWithIdentifier(identifier))
	}

	def bind[T](key : String, value : T) : Unit = {
		value match {
			case m : Moddable[T] => widgetData.bindings += (key -> m)
			case f : (() => T) => widgetData.bindings += (key -> Moddable(f))
			case v : T => widgetData.bindings += (key -> Moddable(v))
		}
	}


	private def subResolve(thing : Any, subKeys : Seq[String]) : Option[_] = {
		subKeys.headOption match {
			case Some(nextKey) =>
				val subRes = thing match {
					case Some(x) => subResolve(x, subKeys)
					case m : Map[String, Any] => m.get(nextKey)
					case other =>
						if (ReflectionAssistant.hasField(other, nextKey)) {
							Some(ReflectionAssistant.getFieldValue(other, nextKey))
						} else {
							Noto.debug(s"unsupported sub resolution type in widget binding: $thing")
							None
						}
				}
				subRes.map {
					case m : Moddable[_] => m.resolve()
					case supplier : (() => Any) => supplier()
					case Some(x) => x
					case other => other
				} match {
					case Some(res) => subResolve(res, subKeys.tail)
					case None => None
				}
			case None =>
				thing match {
					case Some(wrappedThing) => Some(wrappedThing)
					case unwrapped => Some(unwrapped)
				}
		}

	}

	def resolveBinding(bindingKey : String) : Option[_] = {
		val splitSections = bindingKey.split('.')

		val accumKey = new StringBuilder
		var remainingKeys = Vector[String]()
		var subResolved : Option[_] = None
		for (subKey <- splitSections ) {
			if (subResolved.isEmpty) {
				accumKey.append(subKey)
				subResolved = widgetData.bindings.get(accumKey.toString()).map(_.resolve())
				accumKey.append('.')
			} else {
				remainingKeys :+= subKey
			}
		}

		subResolved match {
			case Some(thing) => subResolve(thing, remainingKeys)
			case None if widgetData.parent.isSentinel => None
			case _ => widgetData.parent.resolveBinding(bindingKey)
		}

//		widgetData.bindings.get(bindingKey) match {
//			case Some(value) => Some(value.resolve())
//			case None if widgetData.parent.isSentinel => None
//			case _ => widgetData.parent.resolveBinding(bindingKey)
//		}
	}

	/**
	 * Resolves the data (if any) bound to this widget. This only has meaning if you give it meaning, the windowing framework itself does not use it
	 */
	def boundData : Option[_] = {
		widgetData.dataBinding.map(d => resolveBinding(d))
	}

	override def hashCode(): Int = entity.hashCode()

	override def equals(obj: Any): Boolean = obj match {
		case w : Widget => this.entity == w.entity
		case e : Entity => this.entity == e
		case _ => false
	}

	override def toString: String = widgetData.identifier.orElse(widgetData.configIdentifier).getOrElse(s"Widget($entity)")
}


trait WidgetInstance {
	def widget : Widget
}

abstract class WidgetType[T <: WidgetInstance, D <: TWidgetAuxData](implicit classTag : ClassTag[D]) extends WidgetConstructor [T] with TEagerSingleton {
	implicit def toWidget(idw : T) : Widget = idw.widget
	implicit def toSpecificData(idw : T) : D = idw.widget.data[D]
	implicit def toWidgetData(idw : T) : WidgetData = idw.widget.data[WidgetData]
	implicit def toDrawingData(idw : T) : DrawingData = idw.widget.data[DrawingData]

	WidgetType.types += (this.getClass.getSimpleName.replace("$","") -> this)
}

object WidgetType {
	var types : Map[String, WidgetType[_ <: WidgetInstance,_]] = Map()
}

trait WidgetConstructor[T <: WidgetInstance] {
	def initializeWidget(widget : Widget) : T
}

object Widget {
	implicit def toWidgetData(w : Widget) : WidgetData = w.widgetData

	val bindingParser = "%\\(([a-zA-Z0-9.]*)\\)".r
}

class SimpleWidget(w : Widget) extends WidgetInstance {
	override def widget: Widget = w
}
object SimpleWidget extends WidgetType[SimpleWidget, WidgetData] {
	override def initializeWidget(widget: Widget): SimpleWidget = new SimpleWidget(widget)
}

class Div(w : Widget) extends WidgetInstance {
	override def widget: Widget = w
}
object Div extends WidgetType[Div, WidgetData] {
	override def initializeWidget(widget: Widget): Div = new Div(widget)
}

@GenerateCompanion
class WidgetData extends TWidgetAuxData with TEventUser {
	// must be supplied as part of creation
	var widget : Widget = _
	private[windowing] var _parent : Widget = _
	var children : List[Widget] = Nil

	def parent = _parent
	def parent_=(w : Widget) : Unit = topLevelParent.synchronized {
		if (_parent != null && _parent.notSentinel) {
			_parent.widgetData.children = _parent.widgetData.children.without(widget)
			_parent.widgetData.markModified()
		}
		_parent = w
		if (_parent != null && _parent.notSentinel) {
			_parent.widgetData.children ::= widget
			_parent.widgetData.markModified()
		}
	}

	var identifier : Option[String] = None
	var configIdentifier : Option[String] = None
	var bindings : Map[String, Moddable[_]] = Map()
	var dataBinding : Option[String] = None

	var position = Vec3T[PositionExpression](Flow, Flow, Flow)
	var dimensions = Vec2T[DimensionExpression](Intrinsic, Intrinsic)
	var showing = Moddable(true)

	var acceptsFocus = false
	var hasFocus = false

	private[windowing] var markedModified = false
	var modificationCriteria = List[Widget => Boolean]()
	var modificationWatchers = List[Watcher[_]]()

	var notConfigManaged = false

	def isModified = markedModified || modificationCriteria.exists(criteria => criteria(widget)) || modificationWatchers.exists(w => w.hasChanged)
	def markModified() { markedModified = true }
	def unmarkModified() { markedModified = false }

	def hasIdentifier(ident : String) = identifier.contains(ident) || configIdentifier.contains(ident)
	def effectiveIdentifier : String = identifier.orElse(configIdentifier).getOrElse("unidentified")

	def x = position.x
	def x_= (t : PositionExpression) = position.x = t
	def y = position.y
	def y_= (t : PositionExpression) = position.y = t
	def z = position.z
	def z_= (t : PositionExpression) = position.z = t

	def width = dimensions.x
	def width_= (t : DimensionExpression) = dimensions.x = t
	def height = dimensions.y
	def height_= (t : DimensionExpression) = dimensions.y = t

	def selfAndAncestors : List[Widget] = if (_parent.isSentinel) {
		List(widget)
	} else {
		List(widget) ::: _parent.widgetData.selfAndAncestors
	}
	def selfAndChildren : Iterable[Widget] = Iterable(widget) ++ children.flatMap(_.widgetData.selfAndChildren)
	def topLevelParent : Widget = if (_parent == null || _parent.isSentinel) { widget } else { parent.widgetData.topLevelParent }

	def onClose(): Unit = {
		parent = new Widget(Entity.Sentinel, widget.windowingSystem)
	}

	override def modificationSignature: AnyRef = (position, dimensions, showing.resolve())

	override def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {
		for (pos <- configValue.fieldOpt("position")) {
			for (posArr <- pos.arrOpt) {
				for ((cv,idx) <- posArr.toList.zipWithIndex; if idx < 3) {
					PositionExpression.parse(cv.str, widget.parent.children).ifPresent(pe => position(idx) = pe)
				}
			}
		}
		configValue.fieldOpt("x").flatMap(xv => PositionExpression.parse(xv.str, widget.parent.children)).ifPresent(xv => x = xv)
		configValue.fieldOpt("y").flatMap(yv => PositionExpression.parse(yv.str, widget.parent.children)).ifPresent(yv => y = yv)
		configValue.fieldOpt("z").flatMap(zv => PositionExpression.parse(zv.str, widget.parent.children)).ifPresent(zv => z = zv)

		for (dim <- configValue.fieldOpt("dimensions").orElse(configValue.fieldOpt("dim"))) {
			for (dimArr <- dim.arrOpt) {
				for ((cv,idx) <- dimArr.toList.zipWithIndex; if idx < 2) {
					DimensionExpression.parse(cv.str, widget.parent.children).ifPresent(de => dimensions(idx) = de)
				}
			}
		}

		configValue.fieldOpt("width").flatMap(wv => DimensionExpression.parse(wv.str, widget.parent.children)).ifPresent(wv => width = wv)
		configValue.fieldOpt("height").flatMap(hv => DimensionExpression.parse(hv.str, widget.parent.children)).ifPresent(hv => height = hv)

		for (showConf <- configValue.fieldOpt("showing")) {
			showing = showConf.str match {
				case Widget.bindingParser(key) => Moddable(() => widget.resolveBinding(key) match {
					case Some(b) => b match {
						case boolean: Boolean =>
//							Noto.info(s"Resolved showing to $boolean")
							boolean
						case _ =>
							Noto.warn(s"invalid type for showing binding : $b")
							true
					}
					case _ => true
				})
			}
		}

		if (configValue.consumeMouseButtonEvents.boolOrElse(false)) {
			consumeEvent {
				case mpe : MousePressEvent =>
				case mre : MouseReleaseEvent =>
			}
		}

		dataBinding = configValue.fieldOpt("dataBinding").map(_.str).flatMap(s => Widget.bindingParser.findFirstMatchIn(s).map(m => m.group(1)))
		markModified()
	}

}