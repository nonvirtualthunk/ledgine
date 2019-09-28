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
import arx.core.vec.Cardinals.Left
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec4f, Vec2f}
import arx.engine.EngineCore
import arx.engine.control.components.windowing.{Widget, WidgetInstance, WidgetType, WindowingSystem}
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.graphics.TextureBlock
import arx.graphics.helpers.{Color, RichText}
import arx.graphics.text.{HorizontalTextAlignment, TBitmappedFont}
import arx.resource.ResourceManager

import scala.language.implicitConversions


@GenerateCompanion
class TextDisplay extends TWidgetAuxData {
	var text : Moddable[RichText] = Moddable(RichText(""))
	var fontScale = 1.0f
	var fontColor : Moddable[ReadVec4f] = Moddable( Color.Black )
	var font = none[FontWrapper]
	var textAlignment : Moddable[HorizontalTextAlignment] = Moddable(HorizontalTextAlignment.Left)
	var orientFromTop = Moddable(true)

	override def modificationSignature: AnyRef = (text.resolve(), fontScale, fontColor, font)

	def effectiveFontScale = fontScale
//	constructed = true
}

case class TextDisplayWidget(widget : Widget) extends WidgetInstance {

}

object TextDisplayWidget extends WidgetType[TextDisplayWidget, TextDisplay] {

	override def initializeWidget(widget: Widget): TextDisplayWidget = {
		widget.attachData[TextDisplay]
		TextDisplayWidget(widget)
	}
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