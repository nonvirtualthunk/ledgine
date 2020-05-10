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
import arx.engine.data.{ConfigDataLoader, ConfigLoadable, Moddable, TMutableAuxData}
import arx.engine.graphics.data.windowing.ImageDisplay
import arx.graphics.TToImage
import arx.graphics.helpers.Color
import arx.resource.ResourceManager



trait TWidgetAuxData extends TMutableAuxData {
	def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {}

	def modificationSignature : AnyRef = None
	def hasModificationSignature : Boolean = false
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
									var drawOverlay : Moddable[Boolean] = Moddable(true),
									@NoAutoLoad var overlayImage: Moddable[TToImage] = Moddable(ResourceManager.blankImage : TToImage),
									var centerColor: Moddable[Color] = Moddable(Color.White),
									var overlayEdgeColor: Moddable[Color] = Moddable(Color.White),
									var pixelScale: Int = 1,
									var drawCenter: Boolean = false,
									var pixelSizeDelta: ReadVec2i = Vec2i(0,0)
						) extends ConfigLoadable {
}

class OverlayData extends TWidgetAuxData {
	@NoAutoLoad var overlays = Map[AnyRef, WidgetOverlay]()


	override def loadFromConfig(widget: Widget, config: ConfigValue, reload: Boolean): Unit = {
		for (overlayConf <- config.fieldOpt("overlays")) {
			for ((k,v) <- overlayConf.fields) {
				val overlay = WidgetOverlay()
				overlay.loadFromConfig(v)
				v.fieldOpt("overlayImage", "image").foreach(cv => overlay.overlayImage = ConfigLoadingHelper.loadImageFromConfig(cv, widget))
				for (cv <- v.fieldOpt("centerColor"); color <- ConfigLoadingHelper.loadColorFromConfig(cv, widget)) {
					overlay.centerColor = color
				}
				for (cv <- v.fieldOpt("overlayEdgeColor", "edgeColor"); color <- ConfigLoadingHelper.loadColorFromConfig(cv, widget)) {
					overlay.overlayEdgeColor = color
				}
				for (cv <- v.fieldOpt("drawOverlay", "draw")) {
					overlay.drawOverlay = ConfigLoadingHelper.loadBooleanFromConfig(cv, widget, defaultValue = false)
				}
				overlays += k -> overlay
			}
		}
	}

	override def modificationSignature: AnyRef = overlays.map {
		case (k,v) => k -> v.drawOverlay.resolve()
	}
	override def hasModificationSignature : Boolean = true
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
	@NoAutoLoad
	var tintColor : Moddable[Option[Color]] = Moddable(None)

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

	override def modificationSignature: AnyRef = (backgroundColor.resolve(), tintColor.resolve())
	override def hasModificationSignature : Boolean = true

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
		for (tc <- ConfigLoadingHelper.loadColorFromConfig(configValue.tintColor, widget)) {
			tintColor = Moddable(() => Some(tc.resolve()))
		}

		for (overlayConf <- configValue.fieldOpt("overlays")) {
			if (!widget.hasData[OverlayData]) {
				widget.attachData[OverlayData]
			}
			widget.data[OverlayData].loadFromConfig(widget, configValue, reload)
		}
	}
}

object DrawingData {
	val Default = new DrawingData

	val AllEdges = Set(0,1,2,3)
}