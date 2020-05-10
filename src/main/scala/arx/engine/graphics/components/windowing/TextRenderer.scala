package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import java.util.concurrent.atomic.AtomicInteger

import arx.Prelude._
import arx.application.Noto
import arx.core.math.{Rectf, Recti}
import arx.core.metrics.Metrics
import arx.core.vec.{ReadVec2f, ReadVec2i, Vec2f, Vec2i}
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.{TextDisplay, TextDisplayRenderedGlyphData, TextDisplayWidget, TextInputData, TextInputWidget}
import arx.engine.data.Moddable
import arx.engine.graphics.components.DrawPriority
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics
import arx.graphics.{Axis, Image}
import arx.graphics.helpers.{ImageSectionLayer, RGBA, RichText, RichTextModifier, TextSection}
import arx.graphics.text.{HorizontalTextAlignment, TBitmappedFont, TextLayoutResult, TextLayouter, VerticalTextAlignment}
import overlock.atomicmap.AtomicMap

import scala.collection.mutable

class TextRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
	val cachedLayout = AtomicMap.atomicNBHM[(TBitmappedFont, Float, RichText, Rectf), TextLayoutResult]
	val cacheSize = new AtomicInteger(0)

	val layoutCacheMisses = Metrics.counter("TextRenderer.layoutCacheMisses")
	val layoutCounter = Metrics.counter("TextRenderer.layoutRequests")

	def effectiveFontFor(tw : Widget, textDisplay: TextDisplay) = textDisplay.font match {
		case Some(wrapper) => wrapper.font.apply(WD.textureBlock)
		case None => WD.defaultFont
	}

	def layoutFor(tw : Widget, textDisplay: TextDisplay, effFont : TBitmappedFont, text : RichText, area : Rectf) : TextLayoutResult = {
		layoutCounter.inc()

		val ret = cachedLayout.getOrElseUpdate((effFont, textDisplay.effectiveFontScale, text, area), {
			layoutCacheMisses.inc()
			val layouter = TextLayouter(effFont, textDisplay.effectiveFontScale)
			val layout = layouter.layoutText(text, area, textDisplay.textAlignment, textDisplay.verticalTextAlignment)
			cacheSize.incrementAndGet()

			layout
		})

		val renderedData = tw[TextDisplayRenderedGlyphData]
		renderedData.glyphRects = ret.glyphRects
		renderedData.lineHeight = ret.lineHeight

		if (cacheSize.get() > 1000) {
			cachedLayout.clear()
			cacheSize.set(0)
		}

		ret
	}

	def layout(tw : Widget, textDisplay: TextDisplay, area : Rectf) = {
		val font = effectiveFontFor(tw, textDisplay)
		val layout = layoutFor(tw, textDisplay, font, effectiveTextFor(tw, textDisplay), area)

		layout
	}

	def effectiveTextFor(tw : Widget, td : TextDisplay) = {
		tw.dataOpt[TextInputData] match {
			case Some(_) if tw.hasFocus => td.text append TextSection("|", Moddable(RGBA(0.4f,0.4f,0.4f,1.0f)))
			case _ => td.text.resolve()
		}
	}

	override def render(widget: Widget, beforeChildren: Boolean, bounds: Recti): Seq[WQuad] = {
		if (beforeChildren) {
			widget.dataOpt[TextDisplay] match {
				case Some(td) =>
					val renderedData = widget[TextDisplayRenderedGlyphData]
					renderedData.absoluteOffset = widget.drawing.absolutePosition.xy

					val text = effectiveTextFor(widget,td)
					val renderResult = TextRenderer.render(layout(widget, td, widget.drawing.effectiveClientArea.toRectf), text)



					renderResult
				case _ => Nil
			}
		} else { Nil }
	}

	override def intrinsicSize(widget: Widget, fixedX: Option[Int], fixedY: Option[Int]): Option[ReadVec2i] = {
		widget.dataOpt[TextDisplay] match {
			case Some(textDisplay) =>
				if (textDisplay.text.resolve().isEmpty) {
					Some(Vec2i.Zero)
				} else {
					val layoutResult = layout(widget, textDisplay, Rectf(0.0f, 0.0f, fixedX.map(_.toFloat).getOrElse(100000.0f), fixedY.map(_.toFloat).getOrElse(1000000.0f)))
					Some(Vec2i((layoutResult.dimensions.x).toInt + 4, layoutResult.dimensions.y.toInt))
				}
			case _ => None
		}
	}

	override def intrinsicDependencies(widget: Widget, axis: Axis): List[(Widget, Axis)] = {
		if (axis == Axis.Y) { // the intrinsic height is dependent on the effective width, since that determines the available space before a line break
			List(widget -> Axis.X)
		} else { // the intrinsic width is not dependent on anything, however
			Nil
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
		val baseFont = res.font
		val boldFont = baseFont.boldFont.getOrElse(baseFont)

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
				val modifiers = sections.head.modifiersAtIndex(si)
				val x = rect.x
				val y = rect.y
				val w = rect.w
				val h = rect.h
				symbol match {
					case char : Char =>
						if (!char.isWhitespace) {
							val effFont = if (modifiers.contains(RichTextModifier.Bold)) { boldFont } else { baseFont }
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