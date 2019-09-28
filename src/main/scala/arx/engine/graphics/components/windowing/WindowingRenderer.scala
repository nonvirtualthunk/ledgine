package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.{Rectf, Recti}
import arx.core.vec.{ReadVec2f, ReadVec2i, Vec2T, Vec2i}
import arx.engine.control.components.windowing.Widget
import arx.engine.graphics.components.DrawPriority
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.{AVBO, TextureBlock}


abstract class WindowingRenderer(val windowingData : WindowingGraphicsData) {
	def render(widget: Widget, beforeChildren: Boolean, bounds: Recti): Seq[WQuad]
	def renderRaw(vbo: AVBO, textureBlock: TextureBlock, bounds: Recti, offset: ReadVec2i)(widget: Widget, beforeChildren: Boolean): Unit = {}
	def renderCustomVBO(textureBlock: TextureBlock, bounds: Recti, offset: ReadVec2i)(widget: Widget): Option[AVBO] = None

	def intrinsicSize(widget: Widget, fixedX: Option[Int], fixedY: Option[Int]): Option[ReadVec2i] = None

	/**
	 * Return new effective inner and outer bounds (client and self) based on the rendering that this will do, based
	 * on whether the x/y axes are fixed. I.e. decorative borders increase the self dimensions if wrapping an image
	 * display that is showing at actual size but will reduce the client size if displaying inside a fixed dimension
	 * widget.
	 */
	def modifyBounds(widget : Widget, fixedOnAxis : Vec2T[Boolean], baseClientArea : Recti, selfDims : Vec2i) {}

	def drawPriority : DrawPriority
}
