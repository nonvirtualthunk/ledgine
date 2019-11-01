package arx.engine.control.components.windowing.widgets.data

/**
  * TODO: Add javadoc
  */

import arx.core.math.Recti
import arx.core.representation.ConfigValue
import arx.core.vec._
import arx.engine.control.components.windowing.Widget
import arx.engine.data.TMutableAuxData
import arx.graphics.TToImage
import arx.graphics.helpers.Color
import arx.resource.ResourceManager



trait TWidgetAuxData extends TMutableAuxData {
	def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {}

	def modificationSignature : AnyRef = None
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
	var effectiveClientArea : Recti = Recti(0,0,0,0)
	var backgroundEdges : Set[Int] = DrawingData.AllEdges

	var shouldRedraw = false

	var relativePosition : ReadVec3i = Vec3i.Zero
	var absolutePosition : ReadVec3i = Vec3i.Zero
	var effectiveDimensions : ReadVec2i = Vec2i.One

	def clientDim = effectiveClientArea.dimensions
	def clientOffset: ReadVec2i = effectiveClientArea.position

	override def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {
		for (bcv <- configValue.fieldOpt("background")) {
			for (cv <- bcv.fieldOpt("image")) {
				backgroundImage = Some(ResourceManager.image(cv.str))
			}
		}

		for (edges <- configValue.fieldOpt("backgroundEdges").map(e => e.arr.map(_.int).toSet)) {
			backgroundEdges = edges
		}
	}
}

object DrawingData {
	val Default = new DrawingData

	val AllEdges = Set(0,1,2,3)
}