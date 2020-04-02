package arx.graphics.helpers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/1/12
 * Time: 8:24 AM
 * Created by nonvirtualthunk
 */

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import arx.Prelude._
import arx.application.Noto
import arx.core.introspection.ReflectionAssistant
import arx.core.math.Rectf
import arx.core.representation.ConfigValue
import arx.core.vec.{Cardinal, Cardinals, ReadVec2f, ReadVec4f, Vec2f}
import arx.engine.control.components.windowing.widgets.SpriteProvider
import arx.engine.data.Moddable
import arx.engine.entity.{Taxon, Taxonomy}
import arx.graphics.TToImage
import arx.graphics.helpers.RichTextModifier.Bold
import arx.graphics.text.TBitmappedFont


sealed abstract class RichTextSection {
	def symbolAtIndex(i : Int) : Any
	def symbolCount : Int
	def colorAtIndex(i : Int) : Color
	def backgroundColorAtIndex(i : Int) : Option[Color] = None
	def scaleAtIndex(i : Int) : RichTextScale
	def modifiersAtIndex(i : Int) : Vector[RichTextModifier] = Vector()
	def merge (s : RichTextSection) : Option[RichTextSection] = None
	def isEmpty : Boolean = symbolCount == 0
}
case class TextSection(text : String, color : Moddable[Color] = Moddable(Color.Black), backgroundColor : Option[Color] = None, modifiers : Vector[RichTextModifier] = Vector(), scale : Float = 1.0f) extends RichTextSection {
	val scaleValue = RichTextScale.Scale(scale)
	override def symbolAtIndex(i: Int): Any = text(i)
	override def symbolCount: Int = text.length
	override def colorAtIndex(i: Int): Color = color.resolve()
	override def scaleAtIndex(i : Int) : RichTextScale = scaleValue
	override def backgroundColorAtIndex(i : Int) : Option[Color] = backgroundColor
	override def merge (s : RichTextSection) : Option[RichTextSection] = s match {
		case ts : TextSection if ts.color == color => Some(TextSection(text + ts.text, color))
		case _ => None
	}
	override def modifiersAtIndex(i: Int): Vector[RichTextModifier] = modifiers
}
case class HorizontalPaddingSection(width : Int) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = " "
	override def symbolCount: Int = 0
	override def colorAtIndex(i: Int): Color = Color.White
	override def scaleAtIndex(i: Int): RichTextScale = RichTextScale.Default
}
case class EnsureHorizontalSpaceSection(width : Int) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = " "
	override def symbolCount: Int = 0
	override def colorAtIndex(i: Int): Color = Color.White
	override def scaleAtIndex(i: Int): RichTextScale = RichTextScale.Default
}
case class LineBreakSection(gap : Int) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = " "
	override def symbolCount: Int = 0
	override def colorAtIndex(i: Int): Color = Color.White
	override def scaleAtIndex(i: Int): RichTextScale = RichTextScale.Default
}
case class ImageSectionLayer(image : TToImage, color : Color = Color.White)
case class ImageSection(layers : List[ImageSectionLayer], scale : RichTextScale) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = i match {
		case 0 => layers
		case _ => Noto.severeError("Out of bounds access to image rich-text section"); '~'
	}
	override def symbolCount: Int = 1
	override def colorAtIndex(i: Int): Color = layers.head.color
	override def scaleAtIndex(i : Int) : RichTextScale = scale
}
object TaxonSections {
	lazy val spriteLibraries = ReflectionAssistant.instancesOfSubtypesOf[SpriteProvider]

	def apply(taxon: String, settings: RichTextRenderSettings) : List[RichTextSection] = {
		this.apply(Taxonomy(taxon), settings)
	}
	def apply(taxon: Taxon, settings: RichTextRenderSettings): List[RichTextSection] = {
		for (lib <- spriteLibraries; sprite <- lib.getSpriteDefinitionFor(taxon)) {
			val mainSections = ImageSection(sprite.icon, RichTextScale.ScaleToText(true), Color.White)

			return EnsureHorizontalSpaceSection(8) :: mainSections :: EnsureHorizontalSpaceSection(8) :: Nil
		}
		import arx.Prelude._
		List(TextSection(" " + taxon.name.fromCamelCase.capitalizeAll + " ", modifiers = Vector(Bold), scale = settings.scale))
	}
}
object ImageSection {
	def apply(image : TToImage, scale : RichTextScale, color : Color) : ImageSection = ImageSection(ImageSectionLayer(image, color) :: Nil, scale)
	def scaledTo(image : TToImage, scaleTo : Int, color : Color) : ImageSection = {
		val img = image.image
		ImageSection(List(ImageSectionLayer(img, color)), RichTextScale.ScaleTo(scaleTo, integerScale = true))
	}
}

trait RichTextModifier
object RichTextModifier {
	case object Bold extends RichTextModifier
}

sealed trait RichTextScale
object RichTextScale {
	case class Scale(factor : Float) extends RichTextScale
	case class ScaleTo(target : Int, integerScale : Boolean) extends RichTextScale
	case class ScaleToText(integerScale : Boolean) extends RichTextScale

	val Default : RichTextScale = Scale(1.0f)


	val scaleToRegex = "(?i)scale\\s?to\\(([0-9]+)\\)".r
	val scaleToText = "(?i)scale\\s?to\\s?text".r

	def parse(conf : ConfigValue) = {
		if (conf.isNumber) {
			RichTextScale.Scale(conf.float)
		} else if (conf.isEmpty) {
			RichTextScale.Default
		} else if (conf.isStr) {
			conf.str match {
				case scaleToRegex(target) => ScaleTo(target.toInt, true)
				case scaleToText => ScaleToText(true)
				case _ =>
					Noto.warn(s"Invalid RichTextScale: $conf")
					RichTextScale.Default
			}
		} else {
			Noto.warn(s"Invalid RichTextScale: $conf")
			RichTextScale.Default
		}
	}
}

case class RichText (sections : Seq[RichTextSection]) {
	def symbolCount = sections.isum(s => s.symbolCount)
	protected def getFromIndex[T](target : Int, func : (RichTextSection, Int) => T) = {
		var tmpS = sections
		var i = target
		while (tmpS.nonEmpty && i >= tmpS.head.symbolCount) {
			i -= tmpS.head.symbolCount
			tmpS = tmpS.tail
		}
		if (tmpS.nonEmpty) {
			func(tmpS(i), i)
		} else {
			throw new IndexOutOfBoundsException("Attempted to access past the end of a rich text")
		}
	}
	def symbolAtIndex(target : Int) = {
		getFromIndex(target, (s, i) => s.symbolAtIndex(i))
	}
	def colorAtIndex(target : Int) = {
		getFromIndex(target, (s,i) => s.colorAtIndex(i))
	}
	def isEmpty = sections.forall(_.isEmpty)

	def append(text : String) = RichText(sections :+ TextSection(text))
	def append(section : RichTextSection) = RichText(sections :+ section)
	def +(section : RichTextSection) = RichText(sections :+ section)
	def append(other : RichText) = RichText(sections ++ other.sections)
	def ++(other : RichText) = RichText(sections ++ other.sections)
	def ++(other : Seq[RichTextSection]) = RichText(sections ++ other)
}
object RichText {
	def parse(str: String, settings: RichTextRenderSettings): RichText = {
		var sections = Vector[RichTextSection]()
		val accum = new StringBuilder
		var taxonMode = false
		for (c <- str) {
			c match {
				case '[' =>
					taxonMode = true
					if (accum.nonEmpty) {
						if (accum.endsWith(" ")) {
							accum.deleteCharAt(accum.length-1)
						}
						sections :+= TextSection(accum.mkString)
						accum.clear()
					}
				case ']' =>
					taxonMode = false
					if (accum.nonEmpty) {
						sections ++= TaxonSections(accum.mkString, settings)
						accum.clear()
					}
				case other => accum.append(other)
			}
		}
		sections :+= TextSection(accum.mkString)
		RichText(sections)
	}

	def apply(section : RichTextSection) = new RichText(section :: Nil)
	implicit def fromSingleSection (section : RichTextSection) = RichText(section)
	implicit def apply (str : String) : RichText = RichText(List(TextSection(str)))
	val Empty = RichText(Nil)
}

trait THasRichTextRepresentation {
	def toRichText(settings : RichTextRenderSettings) : RichText
}


case class TextLayoutResult (points : Vector[ReadVec2f] , dimensions : ReadVec2f)

class RichTextRenderDetail(val i : Int) extends AnyVal
object RichTextRenderDetail {
	val Low = new RichTextRenderDetail(0)
	val Medium = new RichTextRenderDetail(1)
	val High = new RichTextRenderDetail(2)
}

case class RichTextRenderSettings(noSymbols : Boolean = false, detailLevel : RichTextRenderDetail = RichTextRenderDetail.Medium, scale : Float = 1.0f)