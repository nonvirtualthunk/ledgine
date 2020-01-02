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
import arx.core.introspection.ReflectionAssistant
import arx.core.macros.GenerateCompanion
import arx.core.math.Rectf
import arx.core.representation.ConfigValue
import arx.core.vec.Cardinals.Left
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec4f, Vec2f}
import arx.engine.EngineCore
import arx.engine.control.components.windowing.helpers.ConfigLoadingHelper
import arx.engine.control.components.windowing.{Widget, WidgetInstance, WidgetType, WindowingSystem}
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.entity.Taxon
import arx.graphics.{Image, ScaledImage, TextureBlock}
import arx.graphics.helpers.{Color, HorizontalPaddingSection, ImageSection, RGBA, RichText, RichTextRenderSettings, RichTextSection, THasRichTextRepresentation, TextSection}
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
		val defaultTextOpt = configValue.fieldOpt("defaultText")
		for (defaultText <- defaultTextOpt) {
			this.text = Moddable(RichText(defaultText.str))
		}


		for (fc <- ConfigLoadingHelper.loadColorFromConfig(configValue.fontColor, widget)) {
			fontColor = fc
		}

		for (cv <- configValue.fieldOpt("text")) {
			// plaintext for now
			if (cv.isStr) {
				text = parseToTextSections(widget, cv.str, fontColor)
			} else {
				val subTexts = for (elem <- cv.arr) yield {
					if (elem.hasField("text")) {
						val color = if (elem.color.nonEmpty) {
							ConfigLoadingHelper.loadColorFromConfig(elem.color, widget).getOrElse(fontColor)
						} else {
							fontColor
						}

						parseToTextSections(widget, elem.text.str, color)
					} else if (elem.hasField("image")) {
						val colorM = ConfigLoadingHelper.loadColorFromConfig(elem.color, widget)
						Moddable(() => {
							val color = colorM.map(_.resolve()).getOrElse(Color.White)
							RichText(ImageSection(ResourceManager.image(elem.image.str), elem.scale.floatOrElse(1.0f), color))
						})
					} else if (elem.hasField("horizontalPadding")) {
						Moddable(RichText(HorizontalPaddingSection(elem.horizontalPadding.int)))
					} else {
						Moddable(RichText.Empty)
					}
				}
				text = Moddable(() => {
					RichText(subTexts.flatMap(_.resolve().sections).toSeq)
				})
			}
		}

		for (alignmentConf <- configValue.fieldOpt("textAlignment")) {
			textAlignment = Moddable(HorizontalTextAlignment.parse(alignmentConf.str))
		}
		for (alignmentConf <- configValue.fieldOpt("verticalTextAlignment")) {
			verticalTextAlignment = Moddable(VerticalTextAlignment.parse(alignmentConf.str))
		}
	}


	def parseToTextSections(widget : Widget, text : String, color : Moddable[Color]) : Moddable[RichText] = {
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

			Moddable(() => {
				val strAccum = new StringBuilder
				var richTextSections: Vector[RichTextSection] = Vector()
				for (section <- sections) {
					section match {
						case TextDisplayWidget.Raw(str) => strAccum.append(str)
						case TextDisplayWidget.Bound(key) => {
							for (boundObj <- widget.resolveBinding(key)) {
								boundObj match {
									case img: Image =>
										if (strAccum.nonEmpty) {
											richTextSections :+= TextSection(strAccum.toString(), color)
											strAccum.clear()
										}
										richTextSections :+= ImageSection(img, 1.0f, color.resolve())
									case scaledImage: ScaledImage =>
										if (strAccum.nonEmpty) {
											richTextSections :+= TextSection(strAccum.toString(), color)
											strAccum.clear()
										}
										richTextSections :+= ImageSection(scaledImage.image, scaledImage.scale.x, color.resolve())
									case richText: RichText =>
										if (strAccum.nonEmpty) {
											richTextSections :+= TextSection(strAccum.toString(), color)
											strAccum.clear()
										}
										richTextSections ++= richText.sections
									case renderToRich: THasRichTextRepresentation =>
										if (strAccum.nonEmpty) {
											richTextSections :+= TextSection(strAccum.toString(), color)
											strAccum.clear()
										}
										richTextSections ++= renderToRich.toRichText(RichTextRenderSettings()).sections
									case taxon : Taxon =>
										if (strAccum.nonEmpty) {
											richTextSections :+= TextSection(strAccum.toString(), color)
											strAccum.clear()
										}
										richTextSections :+= (TextDisplayWidget.spriteProviders.findFirstWith(prov => prov.getSpriteDefinitionFor(taxon)) match {
											case Some((_, value)) => ImageSection(value.icon, 2.0f, Color.White)
											case None => TextSection(taxon.name)
										})
									case other => strAccum.append(other.toString)
								}
							}
						}
					}
				}
				if (strAccum.nonEmpty) {
					richTextSections :+= TextSection(strAccum.toString(), color)
				}
				RichText(richTextSections)
			})
		} else {
			Moddable(() => RichText(TextSection(text, color)))
		}
	}
}

case class TextDisplayWidget(widget : Widget) extends WidgetInstance {

}

object TextDisplayWidget extends WidgetType[TextDisplayWidget, TextDisplay] {
	lazy val spriteProviders = ReflectionAssistant.instancesOfSubtypesOf[SpriteProvider]

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