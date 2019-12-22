package arx.engine.control.components.windowing.widgets.data

/**
  * TODO: Add javadoc
  */

import arx.core.NoAutoLoad
import arx.core.math.Recti
import arx.core.representation.ConfigValue
import arx.core.vec._
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.helpers.ConfigLoadingHelper
import arx.engine.data.{Moddable, TMutableAuxData}
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

case class WidgetOverlay(
									var drawOverlay : Boolean = true,
									var overlayImage: TToImage = ResourceManager.blankImage,
									var centerColor: Moddable[Color] = Moddable(Color.White),
									var overlayEdgeColor: Moddable[Color] = Moddable(Color.White),
									var pixelScale: Int = 1,
									var drawCenter: Boolean = false,
									var pixelSizeDelta: ReadVec2i = Vec2i(0,0)
						)

class OverlayData extends TWidgetAuxData {
	var overlays = Map[AnyRef, WidgetOverlay]()
}

class DrawingData extends TWidgetAuxData {

	var drawBackground = true
	var drawAsForegroundBorder = false
	var backgroundImage : Option[TToImage] = None
	var backgroundPixelScale = 1
	@NoAutoLoad
	var backgroundColor = Moddable(Color.White)
	@NoAutoLoad
	var edgeColor = Moddable(Color.White)
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

	override def modificationSignature: AnyRef = (backgroundColor.resolve())

	override def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {
		for (bcv <- configValue.fieldOpt("background")) {
			for (cv <- bcv.fieldOpt("image")) {
				backgroundImage = Some(ResourceManager.image(cv.str))
			}
		}

		for (edges <- configValue.fieldOpt("backgroundEdges").map(e => e.arr.map(_.int).toSet)) {
			backgroundEdges = edges
		}

		for (fc <- ConfigLoadingHelper.loadColorFromConfig(configValue.backgroundColor, widget)) {
			backgroundColor = fc
		}
		for (ec <- ConfigLoadingHelper.loadColorFromConfig(configValue.edgeColor, widget)) {
			edgeColor = ec
		}
	}
}

object DrawingData {
	val Default = new DrawingData

	val AllEdges = Set(0,1,2,3)
}