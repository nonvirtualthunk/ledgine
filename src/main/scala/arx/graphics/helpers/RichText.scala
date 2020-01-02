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
import arx.core.math.Rectf
import arx.core.vec.{Cardinal, Cardinals, ReadVec2f, ReadVec4f, Vec2f}
import arx.engine.data.Moddable
import arx.graphics.TToImage
import arx.graphics.text.TBitmappedFont


sealed abstract class RichTextSection {
	def symbolAtIndex(i : Int) : Any
	def symbolCount : Int
	def colorAtIndex(i : Int) : Color
	def backgroundColorAtIndex(i : Int) : Option[Color] = None
	def scaleAtIndex(i : Int) : Float
	def merge (s : RichTextSection) : Option[RichTextSection] = None
	def isEmpty : Boolean = symbolCount == 0
}
case class TextSection(text : String, color : Moddable[Color] = Moddable(Color.Black), backgroundColor : Option[Color] = None) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = text(i)
	override def symbolCount: Int = text.length
	override def colorAtIndex(i: Int): Color = color.resolve()
	override def scaleAtIndex(i : Int) : Float = 1.0f
	override def backgroundColorAtIndex(i : Int) : Option[Color] = backgroundColor
	override def merge (s : RichTextSection) : Option[RichTextSection] = s match {
		case ts : TextSection if ts.color == color => Some(TextSection(text + ts.text, color))
		case _ => None
	}
}
case class HorizontalPaddingSection(width : Int) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = " "
	override def symbolCount: Int = 0
	override def colorAtIndex(i: Int): Color = Color.White
	override def scaleAtIndex(i: Int): Float = 1.0f
}
case class LineBreakSection(gap : Int) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = " "
	override def symbolCount: Int = 0
	override def colorAtIndex(i: Int): Color = Color.White
	override def scaleAtIndex(i: Int): Float = 1.0f
}
case class ImageSectionLayer(image : TToImage, color : Color = Color.White)
case class ImageSection(layers : List[ImageSectionLayer], scale : Float) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = i match {
		case 0 => layers
		case _ => Noto.severeError("Out of bounds access to image rich-text section"); '~'
	}
	override def symbolCount: Int = 1
	override def colorAtIndex(i: Int): Color = layers.head.color
	override def scaleAtIndex(i : Int) : Float = scale
}
object ImageSection {
	def apply(image : TToImage, scale : Float, color : Color) : ImageSection = ImageSection(ImageSectionLayer(image, color) :: Nil, scale)
	def scaledTo(image : TToImage, scaleTo : Int, color : Color) : ImageSection = {
		val img = image.image
		val scale = (scaleTo / img.height).toInt
		ImageSection(img, scale, color)
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
}
object RichText {
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

case class RichTextRenderSettings(noSymbols : Boolean = false, detailLevel : RichTextRenderDetail = RichTextRenderDetail.Medium)