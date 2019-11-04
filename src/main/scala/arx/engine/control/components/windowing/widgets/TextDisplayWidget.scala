package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
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
import arx.graphics.{Image, TextureBlock}
import arx.graphics.helpers.{Color, ImageSection, RichText, RichTextSection, THasRichTextRepresentation, TextSection}
import arx.graphics.text.{HorizontalTextAlignment, TBitmappedFont}
import arx.resource.ResourceManager

import scala.language.implicitConversions


@GenerateCompanion
class TextDisplay extends TWidgetAuxData {
	var text : Moddable[RichText] = Moddable(RichText(""))
	var fontScale = 1.0f
	var fontColor : Moddable[Color] = Moddable( Color.Black )
	var font = none[FontWrapper]
	var textAlignment : Moddable[HorizontalTextAlignment] = Moddable(HorizontalTextAlignment.Left)
	var orientFromTop = Moddable(true)

	override def modificationSignature: AnyRef = (text.resolve(), fontScale, fontColor, font)

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
												richTextSections :+= TextSection(strAccum.toString())
												strAccum.clear()
											}
											richTextSections :+= ImageSection(img, 1.0f, Color.White)
										case richText : RichText => richTextSections ++= richText.sections
										case renderToRich : THasRichTextRepresentation => richTextSections ++= renderToRich.toRichText.sections
										case other => strAccum.append(other.toString)
									}
								}
							}
						}
					}
					if (strAccum.nonEmpty) {
						richTextSections :+= TextSection(strAccum.toString())
					}
					RichText(richTextSections)
				})
			} else {
				this.text = Moddable(RichText(text))
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