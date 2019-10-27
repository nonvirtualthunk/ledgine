package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.math.{Rectf, Recti}
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec4f, Vec2T, Vec2i, Vec4f}
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.engine.graphics.components.DrawPriority
import arx.engine.graphics.data.windowing.ImageDisplay
import arx.engine.graphics.data.windowing.ImageDisplay.{Center, Scale, ScaleToFit, TopLeft}
import arx.graphics.{AVBO, TextureBlock}
import arx.graphics.helpers.Color
//import arx.engine.control.components.windowing.widgets.ImageDisplayWidget
//import arx.engine.control.components.windowing.widgets.ImageDisplayWidget.{ActualSize, Center, ScaleToFit, TopLeft}
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.{Image, TToImage}
import arx.resource.ResourceManager

class BackgroundRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
	case class ImageMetric ( borderPixelWidth : Int , centerColor : Color )
	val imageMetrics = memoize( (image:Image,seg:Boolean) => {
		if ( image.sentinel ) {
			ImageMetric(0,Color.White)
		} else {
			var borderWidth = 0
			while ( borderWidth < image.width && image(borderWidth,image.height / 2,3) > 0 ) { borderWidth += 1}
			val centerColor = image.colorAtV4( image.width - 1 , 0 )

			if (seg && borderWidth >= image.width - 1) { Noto.warn("Old style segmented image detected, " + image.resourcePath) }
			ImageMetric(borderWidth, centerColor)
		}
	} )
	val BlankImage : TToImage = ResourceManager.image("default/blank.png")

	override def render(widget: Widget, beforeChildren: Boolean, bounds: Recti): List[WQuad] = {
		val DD = widget.drawing
		import DD._

		val seg = true

		if (drawBackground) {
			val img = backgroundImage.map(_.image).getOrElse(WD.defaultBackgroundImage)

			val ww = bounds.w
			val wh = bounds.h
			val pixelScale = backgroundPixelScale * EngineCore.pixelScaleFactor.toInt

			val metrics = imageMetrics(img,true)

			if (beforeChildren) {
				if (!drawAsForegroundBorder) {
					if ( seg ) {
						val coff = metrics.borderPixelWidth*pixelScale-1 //center offset
						if ( drawCenterBackground ) {
							List(WQuad(Rectf(bounds.x + coff, bounds.y + coff, ww - coff*2, wh - coff*2), BlankImage, metrics.centerColor.asRGBA * backgroundColor.asRGBA))
						} else {
							Nil
						}
					} else {
						List(WQuad(Rectf(bounds),img,backgroundColor))
					}
				} else {
					Nil
				}
			} else {
				var cornerWidth = (img.width / 2) * pixelScale
				var cornerHeight = (img.height / 2) * pixelScale
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
				val sideWidth = ww - cornerWidth * 2
				val sideHeight = wh - cornerHeight * 2

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

				val cornerTR = Rectf(ctx,cty,ctw,cth)
				var ret = List(
					WQuad(Rectf(bounds.x,bounds.y,cornerWidth,cornerHeight),img, edgeColor, flipX = false, flipY = false, 0, cornerTR),
					WQuad(Rectf(bounds.x + ww - cornerWidth,bounds.y,cornerWidth,cornerHeight),img, edgeColor, flipX = true, flipY = false, 0, cornerTR),
					WQuad(Rectf(bounds.x + ww - cornerWidth,bounds.y + wh - cornerHeight,cornerWidth,cornerHeight), img, edgeColor, flipX = true, flipY = true, 0, cornerTR),
					WQuad(Rectf(bounds.x,bounds.y + wh - cornerHeight,cornerWidth,cornerHeight), img, edgeColor, flipX = false, flipY = true, 0, cornerTR)
				)

				if ( ww > cornerWidth * 2 ) {
					ret ::= WQuad(Rectf(bounds.x + cornerWidth,bounds.y,sideWidth,cornerHeight),img,edgeColor, flipX = false, flipY = false, 0, Rectf(hstx,hsty,hstw,hsth))
					ret ::= WQuad(Rectf(bounds.x + cornerWidth,bounds.y + wh - cornerHeight, sideWidth,cornerHeight),img,edgeColor, flipX = false, flipY = true, 0, Rectf(hstx,hsty,hstw,hsth))
				}

				if ( wh > cornerHeight * 2 ) {
					ret ::= WQuad(Rectf(bounds.x,cornerHeight,bounds.y + cornerWidth,sideHeight),img,edgeColor, flipX = false, flipY = false, 0, Rectf(vstx,vsty,vstw,vsth))
					ret ::= WQuad(Rectf(bounds.x + ww - cornerWidth,bounds.y + cornerHeight,cornerWidth,sideHeight),img,edgeColor, flipX = true, flipY = false, 0, Rectf(vstx,vsty,vstw,vsth))
				}

				ret.reverse
			}
		} else {
			Nil
		}
	}

	override def modifyBounds(widget: Widget, fixedOnAxis: Vec2T[Boolean], clientArea: Recti, selfDims: Vec2i) : Unit = {
		val DD = widget.drawing
		if (DD.drawBackground) {
			val img = DD.backgroundImage.map(_.image).getOrElse(WD.defaultBackgroundImage)
			val pixelScale = DD.backgroundPixelScale * EngineCore.pixelScaleFactor.toInt
			val metrics = imageMetrics(img,true)

			val pixelWidth = metrics.borderPixelWidth * pixelScale
			clientArea.x += pixelWidth
			clientArea.y += pixelWidth

			if (fixedOnAxis.x) {
				clientArea.width -= pixelWidth * 2
			} else {
				selfDims.x += pixelWidth * 2
			}
			if (fixedOnAxis.y) {
				clientArea.height -= pixelWidth * 2
			} else {
				selfDims.y += pixelWidth * 2
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
			}
			List(WQuad(rect, img, idd.color))
		case _ => Nil
	}

	override def intrinsicSize(widget: Widget, fixedX: Option[Int], fixedY: Option[Int]): Option[ReadVec2i] = widget.dataOpt[ImageDisplay] match {
		case Some(idd) => idd.scalingStyle match {
			case Scale(scale) =>
				val img : Image = idd.image.resolve()
				Some(Vec2i((img.width * scale).toInt, (img.height * scale).toInt))
			case _ =>
				None
		}
		case _ => None
	}

	override def drawPriority: DrawPriority = DrawPriority.Standard
}