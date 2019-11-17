package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.NoAutoLoad
import arx.engine.data.Moddable
import arx.core.datastructures.Watcher
import arx.core.function.MemoizingFunction
import arx.core.macros.GenerateCompanion
import arx.core.math.Rectf
import arx.core.representation.ConfigValue
import arx.core.vec.Cardinals.Left
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec4f, Vec2f}
import arx.engine.EngineCore
import arx.engine.control.components.windowing.{Widget, WidgetInstance, WidgetType, WindowingSystem}
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.graphics.{Image, ScaledImage, TextureBlock}
import arx.graphics.helpers.{Color, ImageSection, RGBA, RichText, RichTextSection, THasRichTextRepresentation, TextSection}
import arx.graphics.text.{HorizontalTextAlignment, TBitmappedFont, VerticalTextAlignment}
import arx.resource.ResourceManager

import scala.language.implicitConversions


@GenerateCompanion
class TextDisplay extends TWidgetAuxData {
	var text : Moddable[RichText] = Moddable(RichText(""))
	var fontScale = 1.0f
	@NoAutoLoad
	var fontColor : Moddable[Color] = Moddable( Color.Black )
	var font = none[FontWrapper]
	var textAlignment : Moddable[HorizontalTextAlignment] = Moddable(HorizontalTextAlignment.Left)
	var verticalTextAlignment : Moddable[VerticalTextAlignment] = Moddable(VerticalTextAlignment.Bottom)
	var orientFromTop = Moddable(true)

	override def modificationSignature: AnyRef = (text.resolve(), fontScale, fontColor.resolve(), font, textAlignment, verticalTextAlignment)

	def effectiveFontScale = fontScale
//	constructed = true
	override def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {
		val defaultText = configValue.field("defaultText").strOrElse("")
		this.text = Moddable(RichText(defaultText))

		for (cv <- configValue.fieldOpt("text")) {
			// plaintext for now
			val text = cv.str
			val bindingIter = Widget.bindingParser.findAllMatchIn(text)
			if (bindingIter.hasNext) {
				var sections = Vector[TextDisplayWidget.TextResolutionPiece]()

				var cursor = 0
				for (rmatch <- bindingIter) {
					if (rmatch.start > cursor) {
						sections :+= TextDisplayWidget.Raw(text.substring(cursor, rmatch.start))
					}
					sections :+= TextDisplayWidget.Bound(rmatch.group(1))
					cursor = rmatch.end
				}
				if (cursor < text.length) {
					sections :+= TextDisplayWidget.Raw(text.substring(cursor))
				}

				this.text = Moddable(() => {
					val strAccum = new StringBuilder
					var richTextSections : Vector[RichTextSection] = Vector()
					for (section <- sections) {
						section match {
							case TextDisplayWidget.Raw(str) => strAccum.append(str)
							case TextDisplayWidget.Bound(key) => {
								for (boundObj <- widget.resolveBinding(key)) {
									boundObj match {
										case img: Image =>
											if (strAccum.nonEmpty) {
												richTextSections :+= TextSection(strAccum.toString(), fontColor.resolve())
												strAccum.clear()
											}
											richTextSections :+= ImageSection(img, 1.0f, fontColor.resolve())
										case scaledImage: ScaledImage =>
											if (strAccum.nonEmpty) {
												richTextSections :+= TextSection(strAccum.toString(), fontColor.resolve())
												strAccum.clear()
											}
											richTextSections :+= ImageSection(scaledImage.image, scaledImage.scale.x, fontColor.resolve())
										case richText : RichText => richTextSections ++= richText.sections
										case renderToRich : THasRichTextRepresentation => richTextSections ++= renderToRich.toRichText.sections
										case other => strAccum.append(other.toString)
									}
								}
							}
						}
					}
					if (strAccum.nonEmpty) {
						richTextSections :+= TextSection(strAccum.toString(), fontColor.resolve())
					}
					RichText(richTextSections)
				})
			} else {
				this.text = Moddable(RichText(text))
			}
		}

		for (alignmentConf <- configValue.fieldOpt("textAlignment")) {
			textAlignment = Moddable(HorizontalTextAlignment.parse(alignmentConf.str))
		}
		for (alignmentConf <- configValue.fieldOpt("verticalTextAlignment")) {
			verticalTextAlignment = Moddable(VerticalTextAlignment.parse(alignmentConf.str))
		}

		for (cv <- configValue.fieldOpt("fontColor")) {
			if (cv.isStr) {
				cv.str match {
					case Widget.bindingParser(binding) =>
						fontColor = Moddable(() => widget.resolveBinding(binding) match {
							case Some(boundValue) => boundValue match {
								case color: Color => color
								case v: ReadVec4f => RGBA(v)
								case other =>
									Noto.warn(s"invalid bound value for an image display color : $other")
									Color.White
							}
							case None => Color.White
						})
					case _ => // do nothing
				}
			} else {
				fontColor = Moddable(RGBA(cv.v4))
			}
		}
	}
}

case class TextDisplayWidget(widget : Widget) extends WidgetInstance {

}

object TextDisplayWidget extends WidgetType[TextDisplayWidget, TextDisplay] {

	override def initializeWidget(widget: Widget): TextDisplayWidget = {
		widget.attachData[TextDisplay]
		TextDisplayWidget(widget)
	}

	trait TextResolutionPiece { def effectiveText(w : Widget) : String }
	case class Bound(key : String) extends TextResolutionPiece { override def effectiveText(w: Widget): String = w.resolveBinding(key).map(_.toString).getOrElse("") }
	case class Raw(value : String) extends TextResolutionPiece { override def effectiveText(w: Widget): String = value }
}

trait FontWrapper {
	val font : MemoizingFunction[TextureBlock, TBitmappedFont] = memoize((tb : TextureBlock) => createFont(tb))
	protected def createFont(tb : TextureBlock) : TBitmappedFont
}
class FontByName(name : String) extends FontWrapper {
	override def createFont(tb: TextureBlock): TBitmappedFont = {
		ResourceManager.font(name, tb)
	}
}


class TextDisplayRenderedGlyphData extends TWidgetAuxData {
	var glyphRects: Vector[Rectf] = Vector[Rectf]()
	var absoluteOffset : ReadVec2f = Vec2f.Zero
	var lineHeight : Float = 0.0f
}