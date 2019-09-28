package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.math.{Rectf, Recti}
import arx.core.vec.{ReadVec2f, ReadVec2i, Vec2i}
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.{TextDisplay, TextDisplayRenderedGlyphData, TextDisplayWidget}
import arx.engine.graphics.components.DrawPriority
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics
import arx.graphics.Image
import arx.graphics.helpers.{ImageSectionLayer, RichText}
import arx.graphics.text.{HorizontalTextAlignment, TBitmappedFont, TextLayoutResult, TextLayouter, VerticalTextAlignment}

import scala.collection.mutable

class TextRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
	val cachedLayout = new mutable.HashMap[(TBitmappedFont, Float, RichText, Rectf), TextLayoutResult]()


	def effectiveFontFor(tw : Widget, textDisplay: TextDisplay) = textDisplay.font match {
		case Some(wrapper) => wrapper.font.apply(WD.textureBlock)
		case None => WD.defaultFont
	}

	def layoutFor(tw : Widget, textDisplay: TextDisplay, effFont : TBitmappedFont, text : RichText, area : Rectf) : TextLayoutResult = {


		val ret = cachedLayout.getOrElseUpdate((effFont, textDisplay.effectiveFontScale, text, area), {
			val layouter = TextLayouter(effFont, textDisplay.effectiveFontScale)
			val layout = layouter.layoutText(text, area, textDisplay.textAlignment)

			layout
		})

		val renderedData = tw[TextDisplayRenderedGlyphData]
		renderedData.glyphRects = ret.glyphRects
		renderedData.lineHeight = ret.lineHeight

		if (cachedLayout.size > 100) {
			cachedLayout.clear()
		}

		ret
	}

	def layout(tw : Widget, textDisplay: TextDisplay, area : Rectf) = {
		val font = effectiveFontFor(tw, textDisplay)
		val layout = layoutFor(tw, textDisplay, font, textDisplay.text, area)

		layout
	}

	override def render(widget: Widget, beforeChildren: Boolean, bounds: Recti): Seq[WQuad] = {
		if (beforeChildren) {
			widget.dataOpt[TextDisplay] match {
				case Some(td) =>
					val renderedData = widget[TextDisplayRenderedGlyphData]
					renderedData.absoluteOffset = bounds.xy

					val text = td.text
					val renderResult = TextRenderer.render(layout(widget, td, Rectf(bounds.x+2, bounds.y, bounds.w, bounds.h)), text)

					renderResult
				case _ => Nil
			}
		} else { Nil }
	}

	override def intrinsicSize(widget: Widget, fixedX: Option[Int], fixedY: Option[Int]): Option[ReadVec2i] = {
		widget.dataOpt[TextDisplay] match {
			case Some(textDisplay) =>
				val points = layout(widget, textDisplay, Rectf(0.0f,0.0f,fixedX.map(_.toFloat).getOrElse(100000.0f),fixedY.map(_.toFloat).getOrElse(1000000.0f)))
				Some(Vec2i((points.dimensions.x).toInt+4, points.dimensions.y.toInt))
			case _ => None
		}
	}

	override def drawPriority: DrawPriority = DrawPriority.Standard
}

object TextRenderer {
	def render(layouter : TextLayouter, richText : RichText, area : Rectf, horizontalAlignment : HorizontalTextAlignment, verticalAlignment : VerticalTextAlignment) : Vector[WQuad] = {
		val res = layouter.layoutText(richText, area, horizontalAlignment)
		render(res, richText)
	}

	def render(layout : TextLayoutResult, richText : RichText) : Vector[WQuad] = {
		val res = layout
		val effFont = res.font

		var si = 0
		var pi = 0
		var ret = Vector[WQuad]()
		var sections = richText.sections
		while (sections.nonEmpty) {
			if (si >= sections.head.symbolCount) {
				sections = sections.tail
				si = 0
			} else {
				val rect = res.glyphRects(pi)
				val symbol = sections.head.symbolAtIndex(si)
				val x = rect.x
				val y = rect.y
				val w = rect.w
				val h = rect.h
				symbol match {
					case char : Char =>
						if (!char.isWhitespace) {
							val color = sections.head.colorAtIndex(si)
//							val rawW = layouter.charWidth(char,effFont,effectiveFontScale)
//							val rawH = layouter.charHeight(char,effFont,effectiveFontScale)
//							val w = floorf( rawW + 0.0001f)
//							val h = floorf( rawH + 0.0001f)
							//	TODO: bounds culling optimization
							//							if ( y < bounds.y + bounds.h && y + h > bounds.y ) {
							val tc = effFont.characterTexCoords(char)

							for (backgroundColor <- sections.head.backgroundColorAtIndex(si)) {
								ret :+= WQuad(Rectf(x,y,w,effFont.fontMetrics.lineHeight), "default/blank.png", backgroundColor)
							}
							ret :+= WQuad(rect,Image.Sentinel,color,flipX = false, flipY = false, 0,WQuad.StandardRect, Some(tc))
						}
					case layers : List[ImageSectionLayer] =>
						for (layer <- layers) {
							val img = layer.image
//							val w = img.width * scale
//							val h = img.height * scale
							ret :+= WQuad(rect, img, layer.color)
						}
				}

				pi += 1
				si += 1
			}
		}
		ret
	}
}


//object PixelLayouter extends TextLayouter {
//	override def charWidth(char: Char, font: TBitmappedFont, fontScale: Float): Float = {
//		font.characterWidthPixels(char) / EngineCore.pixelScaleFactor * fontScale
//	}
//
//	override def charHeight(char: Char, font: TBitmappedFont, fontScale: Float): Float = {
//		font.characterHeightPixels(char) / EngineCore.pixelScaleFactor * fontScale
//	}
//
//	override def lineHeight(font: TBitmappedFont, fontSize: Float): Float = font.lineHeightPixels / EngineCore.pixelScaleFactor * fontSize
//
//
//	override def maxAscentPlusDescent(font: TBitmappedFont, fontSize: Float): Float = font.maxAscentPlusDescentPixels / EngineCore.pixelScaleFactor * fontSize
//
//
//	override def descent(font: TBitmappedFont, fontSize: Float): Float = font.descentPixels / EngineCore.pixelScaleFactor * fontSize
//
//	override def spaceSize(font : TBitmappedFont, fastFontSize: Float): Float = font.lineHeightPixels / EngineCore.pixelScaleFactor * 0.5f
//}