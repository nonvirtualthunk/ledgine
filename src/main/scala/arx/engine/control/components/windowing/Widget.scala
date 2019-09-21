package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
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

	def withWidgetData(f : WidgetData => Unit) : Unit = { f(this.data[WidgetData]) }
	def withDrawingData(f : DrawingData => Unit) : Unit = { f(this.data[DrawingData]) }


	def createChild[U <: WidgetInstance, D <: TWidgetAuxData](ofType: WidgetConstructor[U]) : U = {
		val w = windowingSystem.createWidget()
		w.parent = this
		ofType.initializeWidget(w)
	}
}


trait WidgetInstance {
	def widget : Widget
}

abstract class WidgetType[T <: WidgetInstance, D <: TWidgetAuxData](implicit classTag : ClassTag[D]) extends WidgetConstructor [T] {
	implicit def toWidget(idw : T) : Widget = idw.widget
	implicit def toSpecificData(idw : T) : D = idw.widget.data[D]
	implicit def toWidgetData(idw : T) : WidgetData = idw.widget.data[WidgetData]
	implicit def toDrawingData(idw : T) : DrawingData = idw.widget.data[DrawingData]
}

trait WidgetConstructor[T <: WidgetInstance] {
	def initializeWidget(widget : Widget) : T
}

object Widget {
	implicit def toWidgetData(w : Widget) : WidgetData = w.widgetData
}

class WidgetData extends TWidgetAuxData with TEventUser {
	// must be supplied as part of creation
	var widget : Widget = _
	private var _parent : Widget = _
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

	private var markedModified = false
	var modificationCriteria = List[(Widget) => Boolean]()

	def isModified = markedModified || modificationCriteria.exists(criteria => criteria(widget))
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
}