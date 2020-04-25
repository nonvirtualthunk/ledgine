package arx.graphics.text

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import arx.application.Noto
import arx.core.math.Rectf
import arx.core.vec.{ReadVec2f, Vec2f, Vec2i}
import arx.graphics.helpers.{EnsureHorizontalSpaceSection, HorizontalPaddingSection, ImageSection, LineBreakSection, RichText, RichTextScale, TextSection}
import arx.Prelude._
import arx.engine.EngineCore
import arx.graphics.helpers.RichTextModifier.Bold
import arx.graphics.text.VerticalTextAlignment.{Bottom, Top}

import scala.collection.mutable.ArrayBuffer

class TextLayouter protected[text](val baseFont : TBitmappedFont, val scale : Float) {
//	protected[text] val fm = font.fontMetrics
	protected[text] var spacingMultiple = 1.0f
	protected[text] var minSpacing = 0.0f

	def withSpacingMultiple(x : Float): TextLayouter = {
		spacingMultiple = x
		this
	}
	def withMinSpacing(x : Float): TextLayouter = {
		minSpacing = x
		this
	}

	def lineHeight(font : TBitmappedFont): Float = font.fontMetrics.lineHeight * scale
	def spaceSize(font : TBitmappedFont): Float = font.fontMetrics.spaceSize * scale
	def tabSize(font : TBitmappedFont): Float = spaceSize(font) * 3
	def descent(font : TBitmappedFont): Float = font.fontMetrics.maxDescent * scale

	def layoutText (text : RichText, area : Rectf, horizontalAlignment : HorizontalTextAlignment = HorizontalTextAlignment.Left, verticalAlignment : VerticalTextAlignment = Bottom) : TextLayoutResult = {
		var x = 0.0f
		var y = 0.0f
		var maxX = 0.0f
		var maxY = 0.0f
		var minRectY = 0.0f
		var maxNonWSX = 0.0f
		var wsX = 0.0f

		var maxSpacingThisLine = 0.0f

		var rects: Vector[Rectf] = Vector()
		val lineRectStartIndices : ArrayBuffer[Int] = ArrayBuffer(0)

		def jumpToNextLine(font : TBitmappedFont): Unit = {
			y += lineHeight(font).max(maxSpacingThisLine)
			x = 0.0f
			maxNonWSX = 0.0f
			wsX = 0.0f
			maxY = math.max(maxY, y)
			maxSpacingThisLine = 0.0f
			lineRectStartIndices.append(rects.length)
		}

		def jumpIfNecessary(font : TBitmappedFont, w : Float): Unit = {
			if (x + w >= area.w && x > area.x + 0.00001f) {
				jumpToNextLine(font)
			}
		}

		def addRect(rect : Rectf, ws : Boolean): Unit = {
			rects :+= rect
			minRectY = minRectY.min(rect.y)
			if (!ws) {
				maxNonWSX = rect.maxX - area.x
			}
		}

		def renderWord(word: String, sectionScale : Float, font : TBitmappedFont) {
			if (word.nonEmpty) {
				val frc = new FontRenderContext(AffineTransform.getScaleInstance(scale * EngineCore.pixelScaleFactor * sectionScale, scale * EngineCore.pixelScaleFactor * sectionScale), !font.pixelFont, !font.pixelFont)

				val glyphVector = font.font.layoutGlyphVector(frc, word.toCharArray, 0, word.length, Font.LAYOUT_LEFT_TO_RIGHT)
				val wordWidth = glyphVector.getGlyphPixelBounds(glyphVector.getNumGlyphs - 1, frc, 0, 0).x.toFloat +
					font.characterWidthPixels(word(glyphVector.getNumGlyphs - 1)) * scale * sectionScale

				jumpIfNecessary(font, wordWidth)

				for (i <- 0 until glyphVector.getNumGlyphs) {
					val glyphRect = glyphVector.getGlyphPixelBounds(i, frc, 0, 0)
					if (x == area.x && glyphRect.getX < 0.0) {
						x += -glyphRect.getX.toFloat
					}
					addRect(Rectf(area.x + x + glyphRect.getX.toFloat, area.y + y, font.characterWidthPixels(word(i)) * scale * sectionScale, font.characterHeightPixels(word(i)) * scale * sectionScale), false)
				}

				x += wordWidth
				wsX = x
				maxX = maxX.max(x)
			}
		}

		for (section <- text.sections) {
			val boldFont = baseFont.boldFont.getOrElse(baseFont)

			val pre = rects.size
			section match {
				case TextSection(text, _, _, modifiers, sectionScale) =>
					val effFont = if (modifiers.contains(Bold)) {
						boldFont
					} else {
						baseFont
					}
					val words = text.split(TextLayouter.whitespaceArray)
					var i = 0

					def advanceThroughWitespace(font : TBitmappedFont): Unit = {
						while (i < text.length && TextLayouter.whitespaceSet.contains(text(i))) {
							text(i) match {
								case ' ' =>
									val sw = spaceSize(font) * spacingMultiple * sectionScale
									addRect(Rectf(area.x + wsX,area.y + y, sw, lineHeight(font)), true)
									wsX += sw
									x = x.max(wsX)
								case '\n' => {
									jumpToNextLine(font)
									addRect(Rectf(area.x + x,area.y + y, 0.0f, lineHeight(font)), true)
								}
								case '\t' =>
									val tw = tabSize(font) * spacingMultiple * sectionScale
									addRect(Rectf(area.x + wsX,area.y + y, tw, lineHeight(font)), true)
									wsX += tw
									x = x.max(wsX)
								case _ => Noto.error("AAAAH")
							}
							maxX = math.max(maxX,x)
							i += 1
						}
					}

					advanceThroughWitespace(effFont)
					for (word <- words) {
						if (!word.isEmpty) {
							renderWord(word, sectionScale, effFont)
							i += word.length
						}
						advanceThroughWitespace(effFont)
					}
				case ImageSection(layers, imgScale) =>
					val baseW = layers.imax(l => l.image.width)
					val baseH = layers.imax(l => l.image.height)

					val dims = imgScale match {
						case RichTextScale.Scale(factor) =>
							Vec2i((baseW * scale * factor).toInt, (baseH * scale * factor).toInt)
						case RichTextScale.ScaleTo(target, integerScale) =>
							val maxWH = baseW max baseH
							val rawFract = (target.toFloat * scale) / maxWH.toFloat
							val fract = if (integerScale) {
								rawFract.floor
							} else {
								rawFract
							}
							Vec2i((baseW * fract).toInt, (baseH * fract).toInt)
						case RichTextScale.ScaleToText(integerScale, multiplier) =>
							val targetY = lineHeight(baseFont) * 0.8 * multiplier
							val rawFract = targetY.toFloat / baseH.toFloat
							val fract = if (integerScale) {
								rawFract.floor.max(1)
							} else {
								rawFract
							}
							Vec2i((baseW * fract).toInt, (baseH * fract).toInt)
					}
					val width = dims.x
					val height = dims.y
					//If this is the only thing on the line and it's longer than the line, no point skipping down, otherwise
					jumpIfNecessary(baseFont, width)
					maxSpacingThisLine = height // ensure that we don't collide with this image on the next line

					addRect(Rectf(area.x + x,(area.y + y + (lineHeight(baseFont) - height) / 2).toInt, width, height), false)

					x += width + minSpacing
					wsX = x
					maxX = math.max(maxX,x)
				case HorizontalPaddingSection(width) =>
					x += width
					wsX = x
					maxX = math.max(maxX,x)
				case EnsureHorizontalSpaceSection(width) =>
					// only shift over if there is something other than whitespace to our left
					if (maxNonWSX > 0.0f) {
						x = x.max(maxNonWSX + width)
					}
				case LineBreakSection(gap) =>
					jumpToNextLine(baseFont)
					y += gap


			}
			if (pre + section.symbolCount != rects.size) {
				println("wrong")
			}
		}

		if (minRectY < 0.0f) {
			rects.foreach(r => r.y += -minRectY + 10)
		}

		if (horizontalAlignment != HorizontalTextAlignment.Left && rects.nonEmpty) {
			for (lineNumber <- 0 until lineRectStartIndices.size) {
				val lineStart = lineRectStartIndices(lineNumber)
				val lineEnd = if (lineNumber < lineRectStartIndices.size - 1) {
					lineRectStartIndices(lineNumber + 1)
				} else {
					rects.size
				}

				if (lineEnd > lineStart) {
					val minX = rects(lineStart).minX
					val maxX = rects(lineEnd - 1).maxX
					val lineWidth = maxX - minX
					val adjustedStartX = area.x + (if (horizontalAlignment == HorizontalTextAlignment.Centered) {
						(area.w - lineWidth) / 2.0f
					} else if (horizontalAlignment == HorizontalTextAlignment.Right) {
						area.w - lineWidth
					} else {
						Noto.warn(s"invalid horizontal text alignment $horizontalAlignment"); minX
					})

					val delta = adjustedStartX - minX
					for (i <- lineStart until lineEnd) {
						rects(i).x += delta
					}
				}
			}
		}

		if (verticalAlignment != VerticalTextAlignment.Bottom && rects.nonEmpty) {
			val minY = rects.head.minY
			val maxY = rects.last.maxY
			val totalHeight = (maxY - minY).max(lineHeight(baseFont))
			val adjustedStartY = area.y + (if (verticalAlignment == VerticalTextAlignment.Centered) {
				(area.h - totalHeight) / 2.0f
			} else if (verticalAlignment == VerticalTextAlignment.Top) {
				area.h - totalHeight
			} else { Noto.warn(s"invalid vertical text alignment $verticalAlignment"); minY })

			val delta = adjustedStartY - minY
			rects.foreach(_.y += delta)
		}

//		if ( rects.nonEmpty && horizontalAlignment == HorizontalTextAlignment.Centered) {
//			val lastSymbolWidth = widths.last
//			val minX = rects.fmin(_.x - area.x)
//			val maxX = rects.fmax(_.x - area.x)
//			val minY = rects.fmin(_.y - area.y)
//			val maxY = rects.fmax(_.y - area.y)
//			val ox = (area.width - (maxX - minX + lastSymbolWidth)) * 0.5f
//			val oy = (area.height - (maxY - minY + lastSymbolWidth)) * 0.5f
//			for (p <- rects) { p.x += ox; p.y += oy }
//		} else if ( rects.nonEmpty && horizontalAlignment == HorizontalTextAlignment.Right) {
//			var maxXForLine = 0.0f
//			var lastY = rects.head.y
//			var rectsOnLine = Vector[Rectf]()
//			def adjustLine (): Unit = {
//				val offset = (area.x + area.w) - maxXForLine
//				for (lp <- rectsOnLine) {
//					lp.x += offset
//				}
//				rectsOnLine = Vector()
//				maxXForLine = 0.0f
//			}
//
//			var i = 0
//			while (i < rects.size) {
//				val p = rects(i)
//				val w = widths(i)
//				if (p.y != lastY) {
//					adjustLine()
//				}
//				rectsOnLine :+= p
//				maxXForLine = math.max(p.x + w,maxXForLine)
//				lastY = p.y
//				i += 1
//			}
//			adjustLine()
//		}

		val lastLineHeight = lineHeight(baseFont) //+ descent + 4
		TextLayoutResult(rects,Vec2f(maxX,maxY + lastLineHeight.max(maxSpacingThisLine)), baseFont, lineHeight(baseFont))
	}
}


case class TextLayoutResult(glyphRects: Vector[Rectf], dimensions: ReadVec2f, font: TBitmappedFont, lineHeight: Float)

object TextLayouter {
	val whitespaceArray: Array[Char] = Array[Char](' ', '\n', '\t')
	val whitespaceSet: Set[Char] = whitespaceArray.toSet


	def apply(baseFont : TBitmappedFont, fontScale : Float) = {
		if (baseFont.pixelFont) {
			new TextLayouter(baseFont, fontScale)
		} else {
			new TextLayouter(baseFont.deriveWithPtSize((baseFont.font.getSize * fontScale).toInt), 1.0f)
		}
	}
}


sealed trait HorizontalTextAlignment
object HorizontalTextAlignment {
	case object Left extends HorizontalTextAlignment
	case object Right extends HorizontalTextAlignment
	case object Centered extends HorizontalTextAlignment

	def parse(str : String) : HorizontalTextAlignment = str.toLowerCase match {
		case "left" => Left
		case "right" => Right
		case "center" | "centered" => Centered
		case _ =>
			Noto.warn(s"Invalid horizontal text alignment $str")
			Left
	}
}

sealed trait VerticalTextAlignment
object VerticalTextAlignment {
	case object Top extends VerticalTextAlignment
	case object Bottom extends VerticalTextAlignment
	case object Centered extends VerticalTextAlignment

	def parse(str : String) : VerticalTextAlignment = str.toLowerCase match {
		case "top" => Top
		case "bottom" => Bottom
		case "center" | "centered" => Centered
		case _ =>
			Noto.warn(s"Invalid horizontal text alignment $str")
			Bottom
	}
}