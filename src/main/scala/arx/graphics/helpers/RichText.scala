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
import arx.core.vec.{Cardinals, ReadVec2f, ReadVec4f, Vec2f}
import arx.graphics.TToImage
import arx.graphics.helpers.TTextLayouter.LayoutParameters
import arx.graphics.text.TBitmappedFont


sealed abstract class RichTextSection {
	def symbolAtIndex(i : Int) : Any
	def symbolCount : Int
	def colorAtIndex(i : Int) : Color
	def backgroundColorAtIndex(i : Int) : Option[Color] = None
	def scaleAtIndex(i : Int) : Float
	def merge (s : RichTextSection) : Option[RichTextSection] = None
}
case class TextSection(text : String, color : Color = Color.Black, backgroundColor : Option[Color] = None) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = text(i)
	override def symbolCount: Int = text.length
	override def colorAtIndex(i: Int): Color = color
	override def scaleAtIndex(i : Int) : Float = 1.0f
	override def backgroundColorAtIndex(i : Int) : Option[Color] = backgroundColor
	override def merge (s : RichTextSection) : Option[RichTextSection] = s match {
		case ts : TextSection if ts.color == color => Some(TextSection(text + ts.text, color))
		case _ => None
	}
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
}
object RichText {
	def apply(section : RichTextSection) = new RichText(section :: Nil)
	implicit def fromSingleSection (section : RichTextSection) = RichText(section)
	implicit def apply (str : String) : RichText = RichText(List(TextSection(str)))
	val Empty = RichText(Nil)
}

trait THasRichTextRepresentation {
	def toRichText : RichText
}

trait TTextLayouter {
	def layOutText (text : RichText, font : TBitmappedFont, fontScale : Float, area : Rectf, spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f, textAlignment :Int = Cardinals.Left) : TextLayoutResult
	def layOutText ( params : LayoutParameters ) : TextLayoutResult = {
		layOutText(params.text,params.font,params.fontScale,params.area,params.spacingMultiple,params.minSpacing,params.textAlignment)
	}



	def textDimensions ( text : RichText , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f) : ReadVec2f
	def textDimensions ( params : LayoutParameters ) : ReadVec2f = {
		textDimensions(params.text,params.font,params.fontScale,params.area,params.spacingMultiple,params.minSpacing)
	}

	def charWidth(char: Char, font: TBitmappedFont, fontScale : Float ): Float
	def charHeight(char: Char, font: TBitmappedFont, fontScale : Float ): Float

	def lineHeight(font : TBitmappedFont, fastFontSize : Float) : Float
}

object TTextLayouter {
	case class LayoutParameters (text : RichText, font : TBitmappedFont, fontScale : Float, area : Rectf, spacingMultiple : Float, minSpacing : Float, textAlignment : Int )
}

case class TextLayoutResult (points : Vector[ReadVec2f] , dimensions : ReadVec2f)