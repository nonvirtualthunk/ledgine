package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.Watcher
import arx.core.introspection.TEagerSingleton
import arx.core.macros.GenerateCompanion
import arx.core.representation.ConfigValue
import arx.core.traits.TSentinelable
import arx.core.vec.{Vec2T, Vec3T}
import arx.engine.control.components.windowing.widgets.DimensionExpression.Intrinsic
import arx.engine.control.components.windowing.widgets.PositionExpression.Flow
import arx.engine.control.components.windowing.widgets.data.{DragAndDropData, DrawingData, TWidgetAuxData}
import arx.engine.control.components.windowing.widgets.{DimensionExpression, ImageDisplayWidget, PositionExpression}
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
		widgetData.onClose()
	}

	def attachData[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.data[T](entity)
	def apply[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.data[T](entity)
	def data[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.data[T](entity)
	def dataOpt[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.dataOpt[T](entity)
	def hasData[T <: TWidgetAuxData](implicit tag : ClassTag[T]) = windowingSystem.displayWorld.view.hasData[T](entity)
	def allData = windowingSystem.displayWorld.allData(entity)

	def withWidgetData(f : WidgetData => Unit) : Unit = { f(this.data[WidgetData]) }
	def withDrawingData(f : DrawingData => Unit) : Unit = { f(this.data[DrawingData]) }


	def createChild[U <: WidgetInstance, D <: TWidgetAuxData](ofType: WidgetConstructor[U]) : U = {
		val w = windowingSystem.createWidget()
		w.parent = this
		ofType.initializeWidget(w)
	}

	def createChild(identifier : String) : Widget = {
		val sections = identifier.split('.')
		if (sections.length == 2) {
			createChild(sections(0), sections(1))
		} else {
			Noto.error("createChild(identifier) expects an identifier in the form of WidgetFile.WidgetKey")
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

	override def hashCode(): Int = entity.hashCode()

	override def equals(obj: Any): Boolean = entity.equals(obj)
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
}

class SimpleWidget(w : Widget) extends WidgetInstance {
	override def widget: Widget = w
}
object SimpleWidget extends WidgetType[SimpleWidget, WidgetData] {
	override def initializeWidget(widget: Widget): SimpleWidget = new SimpleWidget(widget)
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
		if (_parent.notSentinel) {
			_parent.widgetData.children ::= widget
			_parent.widgetData.markModified()
		}
	}

	var identifier : Option[String] = None

	var position = Vec3T[PositionExpression](Flow, Flow, Flow)
	var dimensions = Vec2T[DimensionExpression](Intrinsic, Intrinsic)
	var showing = Moddable(true)

	var acceptsFocus = false
	var hasFocus = false

	private[windowing] var markedModified = false
	var modificationCriteria = List[Widget => Boolean]()
	var modificationWatchers = List[Watcher[_]]()

	def isModified = markedModified || modificationCriteria.exists(criteria => criteria(widget)) || modificationWatchers.exists(w => w.hasChanged)
	def markModified() { markedModified = true }
	def unmarkModified() { markedModified = false }

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

	override def loadFromConfig(configValue: ConfigValue, reload: Boolean): Unit = {
		for (pos <- configValue.fieldOpt("position")) {
			for (posArr <- pos.arrOpt) {
				for ((cv,idx) <- posArr.toList.zipWithIndex; if idx < 3) {
					PositionExpression.parse(cv.str).ifPresent(pe => position(idx) = pe)
				}
			}
		}
		configValue.fieldOpt("x").flatMap(xv => PositionExpression.parse(xv.str)).ifPresent(xv => x = xv)
		configValue.fieldOpt("y").flatMap(yv => PositionExpression.parse(yv.str)).ifPresent(yv => y = yv)

		for (dim <- configValue.fieldOpt("dimensions").orElse(configValue.fieldOpt("dim"))) {
			for (dimArr <- dim.arrOpt) {
				for ((cv,idx) <- dimArr.toList.zipWithIndex; if idx < 2) {
					DimensionExpression.parse(cv.str).ifPresent(de => dimensions(idx) = de)
				}
			}
		}

		configValue.fieldOpt("width").flatMap(wv => DimensionExpression.parse(wv.str)).ifPresent(wv => width = wv)
		configValue.fieldOpt("height").flatMap(hv => DimensionExpression.parse(hv.str)).ifPresent(hv => height = hv)


	}

}