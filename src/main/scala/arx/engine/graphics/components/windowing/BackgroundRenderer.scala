package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.math.{Rectf, Recti, RectiAxis}
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec4f, Vec2T, Vec2i, Vec4f}
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.data.{DrawingData, OverlayData}
import arx.engine.graphics.components.DrawPriority
import arx.engine.graphics.data.windowing.ImageDisplay
import arx.engine.graphics.data.windowing.ImageDisplay.{Center, Scale, ScaleToAxis, ScaleToFit, TopLeft}
import arx.graphics.{AVBO, Axis, TextureBlock}
import arx.graphics.helpers.Color
//import arx.engine.control.components.windowing.widgets.ImageDisplayWidget
//import arx.engine.control.components.windowing.widgets.ImageDisplayWidget.{ActualSize, Center, ScaleToFit, TopLeft}
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.{Image, TToImage}
import arx.resource.ResourceManager

class BackgroundRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
	case class ImageMetric ( borderPixelWidth : Int , centerColor : Color, outerOffset : Int )
	val imageMetrics = memoize( (image:Image,seg:Boolean) => {
		if ( image.sentinel ) {
			ImageMetric(0,Color.White,0)
		} else {
			var outerWidth = 0
			while ( outerWidth < image.width && image(outerWidth,image.height/2,3) == 0) { outerWidth += 1 }
			var borderWidth = outerWidth
			while ( borderWidth < image.width && image(borderWidth,image.height / 2,3) > 0 ) { borderWidth += 1}
			val centerColor = image.colorAtV4( image.width - 1 , 0 )

//			if (seg && borderWidth >= image.width - 1) { Noto.warn("Old style segmented image detected, " + image.resourcePath) }
			ImageMetric(borderWidth, centerColor, outerWidth)
		}
	} )
	val BlankImage : TToImage = ResourceManager.image("default/blank.png")

	case class Corner(enabled : Boolean, width : Float, height : Float)
	override def render(widget: Widget, beforeChildren: Boolean, bounds: Recti): List[WQuad] = {
		val DD = widget.drawing
		import DD._

		val seg = true

		val backgroundQuads = if (drawBackground) {
			val img = backgroundImage.map(_.image).getOrElse(WD.defaultBackgroundImage)

			val pixelScale = backgroundPixelScale * EngineCore.pixelScaleFactor.toInt

			renderNineWayImage(widget, bounds, img, backgroundColor.resolve(), edgeColor.resolve(), backgroundEdges, beforeChildren, drawAsForegroundBorder, seg, pixelScale, drawCenterBackground, Vec2i.Zero)
		} else {
			Nil
		}

		var overlayQuads = List[WQuad]()
		for (overlayData <- widget.dataOpt[OverlayData]; od <- overlayData.overlays.values if od.drawOverlay) {
			val pixelScale = od.pixelScale * EngineCore.pixelScaleFactor.toInt
			overlayQuads :::= renderNineWayImage(widget, bounds, od.overlayImage.resolve(), od.centerColor.resolve(), od.overlayEdgeColor.resolve(), DrawingData.AllEdges, beforeChildren, true, seg, pixelScale, od.drawCenter, od.pixelSizeDelta)
		}

		backgroundQuads ::: overlayQuads
	}

	def renderNineWayImage(widget : Widget, rawBounds : Recti, img : Image, mainColor : Color, edgeColor : Color, edgesActive : Set[Int], beforeChildren : Boolean, drawAsForegroundBorder : Boolean, seg : Boolean, pixelScale : Int, drawCenterBackground : Boolean, pixelSizeDelta : ReadVec2i): List[WQuad] = {
		val bounds = Recti(
			rawBounds.x - pixelSizeDelta.x * pixelScale,
			rawBounds.y - pixelSizeDelta.y * pixelScale,
			rawBounds.width + pixelSizeDelta.x * pixelScale * 2,
			rawBounds.height + pixelSizeDelta.y * pixelScale * 2
		)

		val ww = bounds.w
		val wh = bounds.h

		val metrics = imageMetrics(img,true)

		if (beforeChildren) {
			if (!drawAsForegroundBorder) {
				if ( seg ) {
					val coff = metrics.borderPixelWidth*pixelScale-1 //center offset

					val startX = if (edgesActive(0)) { coff } else { 0.0f }
					val startY = if (edgesActive(1)) { coff } else { 0.0f }
					val endX = if (edgesActive(2)) { ww - coff } else { ww }
					val endY = if (edgesActive(3)) { wh - coff } else { wh }

					if ( drawCenterBackground ) {
						List(WQuad(Rectf(bounds.x + startX, bounds.y + startY, endX - startX, endY - startY), BlankImage, metrics.centerColor.asRGBA * mainColor.asRGBA))
					} else {
						Nil
					}
				} else {
					List(WQuad(Rectf(bounds),img,mainColor))
				}
			} else {
				Nil
			}
		} else {
			var cornerWidth = (img.width / 2) * pixelScale
			var cornerHeight = (img.height / 2) * pixelScale

			val corners = Array.ofDim[Corner](4)
			var q = 0
			while (q < 4) {
				val enabled = edgesActive(q) && edgesActive((q + 1)%4)
				corners(q) = Corner(edgesActive(q) && edgesActive((q + 1)%4), if (enabled) { cornerWidth } else { 0.0f }, if (enabled) { cornerHeight } else { 0.0f })
				q += 1
			}

			// todo : partial edges with small dimensions
			var verticalPercent = 1.0f
			var horizontalPercent = 1.0f
			if ( cornerWidth > ww / 2 ) {
				horizontalPercent = (ww / 2).toFloat / cornerWidth.toFloat
				cornerWidth = roundf(ww / 2).toInt
			}
			if ( cornerHeight > wh / 2 ) {
				verticalPercent = (wh / 2).toFloat / cornerHeight.toFloat
				cornerHeight = roundf(wh / 2).toInt
			}

			//Corner Texture coordinates
			val ctx = 0.0f
			val cty = 1.0f - 0.5f * verticalPercent
			val ctw = 0.5f * horizontalPercent
			val cth = 0.5f * verticalPercent

			//Horizontal Side Texture coordinates
			val hstx = 0.5f
			val hsty = 0.5f + (0.5f * (1.0f - verticalPercent))
			val hstw = 0
			val hsth = cth

			//Vertical Side Texture Coordinates
			val vstx = 0.0f
			val vsty = 0.0f
			val vstw = ctw
			val vsth = 0

			/*
						1
					0		2
						3
			 */


			val cornerTR = Rectf(ctx,cty,ctw,cth)
			var ret = List[WQuad]()
			if (edgesActive(0) && edgesActive(1)) {
				ret ::= WQuad(Rectf(bounds.x,bounds.y,cornerWidth,cornerHeight),img, edgeColor, flipX = false, flipY = false, 0, cornerTR)
			}
			if (edgesActive(1) && edgesActive(2)) {
				ret ::= WQuad(Rectf(bounds.x + ww - cornerWidth,bounds.y,cornerWidth,cornerHeight),img, edgeColor, flipX = true, flipY = false, 0, cornerTR)
			}
			if (edgesActive(2) && edgesActive(3)) {
				ret ::= WQuad(Rectf(bounds.x + ww - cornerWidth,bounds.y + wh - cornerHeight,cornerWidth,cornerHeight), img, edgeColor, flipX = true, flipY = true, 0, cornerTR)
			}
			if (edgesActive(3) && edgesActive(1)) {
				ret ::= WQuad(Rectf(bounds.x,bounds.y + wh - cornerHeight,cornerWidth,cornerHeight), img, edgeColor, flipX = false, flipY = true, 0, cornerTR)
			}


			if ( ww > cornerWidth * 2 ) {
				if (edgesActive(1)) {
					ret ::= WQuad(Rectf(bounds.x + corners(0).width, bounds.y, ww - corners(0).width - corners(1).width, cornerHeight), img, edgeColor, flipX = false, flipY = false, 0, Rectf(hstx, hsty, hstw, hsth))
				}
				if (edgesActive(3)) {
					ret ::= WQuad(Rectf(bounds.x + corners(3).width, bounds.y + wh - cornerHeight, ww - corners(2).width - corners(3).width, cornerHeight), img, edgeColor, flipX = false, flipY = true, 0, Rectf(hstx, hsty, hstw, hsth))
				}
			}

			if ( wh > cornerHeight * 2 ) {
				if (edgesActive(0)) {
					ret ::= WQuad(Rectf(bounds.x, bounds.y + corners(0).height, cornerWidth, wh - corners(0).height - corners(3).height), img, edgeColor, flipX = false, flipY = false, 0, Rectf(vstx, vsty, vstw, vsth))
				}
				if (edgesActive(2)) {
					ret ::= WQuad(Rectf(bounds.x + ww - cornerWidth, bounds.y + corners(1).height, cornerWidth, wh - corners(1).height - corners(2).height), img, edgeColor, flipX = true, flipY = false, 0, Rectf(vstx, vsty, vstw, vsth))
				}
			}

			ret.reverse
		}
	}

	override def modifyBounds(widget: Widget, axis: Axis, fixedOnAxis: Boolean, clientArea: Recti, selfDims: Vec2i): Unit = {
		val DD = widget.drawing
		if (DD.drawBackground) {
			val img = DD.backgroundImage.map(_.image).getOrElse(WD.defaultBackgroundImage)
			val pixelScale = DD.backgroundPixelScale * EngineCore.pixelScaleFactor.toInt
			val metrics = imageMetrics(img,true)

			val pixelWidth = metrics.borderPixelWidth * pixelScale
			axis match {
				case Axis.X =>
					clientArea.x += pixelWidth
					if (fixedOnAxis) {
						clientArea.width -= pixelWidth * 2
					} else {
						selfDims.x += pixelWidth * 2
					}
				case Axis.Y =>
					clientArea.y += pixelWidth
					if (fixedOnAxis) {
						clientArea.height -= pixelWidth * 2
					} else {
						selfDims.y += pixelWidth * 2
					}
				case _ => // do nothing
			}
		}
	}

	override def drawPriority: DrawPriority = DrawPriority.Early
}


class ImageContentRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
	override def render(widget: Widget, beforeChildren: Boolean, bounds: Recti): List[WQuad] = widget.dataOpt[ImageDisplay] match {
		case Some(idd) if beforeChildren =>
			val img : Image = idd.image.resolve()


			val offset = bounds.xy
			val dim = bounds.dimensions

			val rect = idd.scalingStyle match {
				case Scale(scale) =>
					val imgDim = img.dimensions * scale
					idd.positionStyle match {
						case Center =>
							val dimDiff = dim - imgDim
							Rectf((offset.x + dimDiff.x / 2), (offset.y + dimDiff.y / 2), imgDim.x, imgDim.y)
						case TopLeft =>
							Rectf(offset.x, offset.y, imgDim.x	, imgDim.y)
					}
				case ScaleToFit =>
					Rectf(offset.x,offset.y,dim.x,dim.y)
				case ScaleToAxis(axis, size) =>
					val wFract = img.width.toFloat / img.height.toFloat
					axis match {
						case Axis.X => Rectf(offset.x, offset.y, size, size / wFract)
						case Axis.Y => Rectf(offset.x, offset.y, size * wFract, size)
					}

			}
			List(WQuad(rect, img, idd.color))
		case _ => Nil
	}

	override def intrinsicSize(widget: Widget, fixedX: Option[Int], fixedY: Option[Int]): Option[ReadVec2i] = widget.dataOpt[ImageDisplay] match {
		case Some(idd) => idd.scalingStyle match {
			case Scale(scale) =>
				val img : Image = idd.image.resolve()
				Some(Vec2i((img.width * scale).toInt, (img.height * scale).toInt))
			case ScaleToAxis(axis, size) =>
				val img : Image = idd.image.resolve()
				val wFract = img.width.toFloat / img.height.toFloat
				axis match {
					case Axis.X => Some(Vec2i(size, (size / wFract).toInt))
					case Axis.Y => Some(Vec2i((size * wFract).toInt, size))
				}
			case _ =>
				None
		}
		case _ => None
	}

	override def drawPriority: DrawPriority = DrawPriority.Standard
}