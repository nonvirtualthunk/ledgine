package arx.engine.control.components.windowing.widgets.data

/**
  * TODO: Add javadoc
  */

import arx.core.vec._
import arx.engine.data.TMutableAuxData
import arx.graphics.TToImage
import arx.graphics.helpers.Color



trait TWidgetAuxData extends TMutableAuxData {

}

class DragAndDropData extends TWidgetAuxData {
	var draggable = false
	var droppable = false
	var payload : Option[AnyRef] = None
}

object DragAndDropData {
	val Default = new DragAndDropData
}

// +====================+

class DrawingData extends TWidgetAuxData {
	var drawBackground = true
	var drawAsForegroundBorder = false
	var backgroundImage : Option[TToImage] = None
	var backgroundPixelScale = 1
	var backgroundColor = Color.White
	var edgeColor = Color.White
	var drawCenterBackground = true
	// note, these won't necessarily take effect if changed
	var interiorPadding : ReadVec2i = Vec2i.Zero
	var decorationBorderSize : ReadVec2i = Vec2i.Zero

	var shouldRedraw = false

	var relativePosition : ReadVec3i = Vec3i.Zero
	var absolutePosition : ReadVec3i = Vec3i.Zero
	var effectiveDimensions : ReadVec2i = Vec2i.One

	def clientDim = {
		effectiveDimensions - clientOffset * 2
	}

	def clientOffset = {
		interiorPadding + decorationBorderSize
	}

//	var xWatcher = new Watcher[PositionExpression](PositionExpression.Flow)
//	var yWatcher = new Watcher[PositionExpression](PositionExpression.Flow)
//	var zWatcher = new Watcher[PositionExpression](PositionExpression.Flow)
//
//	var widthWatcher = new Watcher[DimensionExpression](DimensionExpression.Intrinsic)
//	var heightWatcher = new Watcher[DimensionExpression](DimensionExpression.Intrinsic)
//
//	override def onAssignedToWidget(widget: Widget): Unit = {
//		xWatcher = new Watcher(widget.x)
//		yWatcher = new Watcher(widget.y)
//		zWatcher = new Watcher(widget.z)
//
//		widthWatcher = new Watcher(widget.width)
//		heightWatcher = new Watcher(widget.height)
//	}
}

object DrawingData {
	val Default = new DrawingData
}