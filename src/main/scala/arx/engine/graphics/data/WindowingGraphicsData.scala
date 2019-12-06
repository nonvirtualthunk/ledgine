package arx.engine.graphics.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.async.Executor
import arx.core.datastructures.MultiMap
import arx.core.mat.Mat4x4
import arx.core.mat.ReadMat4x4
import arx.core.math.Rectf
import arx.core.math.Recti
import arx.core.units.UnitOfTime
import arx.core.vec.{ReadVec3f, Vec2T, Vec3f}
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.PositionExpression
import arx.engine.data.{TMutableAuxData, TWorldAuxData}
import arx.engine.graphics.components.windowing.WQuad
import arx.graphics.AVBO
import arx.graphics.GL
import arx.graphics.TextureBlock
import arx.graphics.pov.TCamera
import arx.graphics.pov.TopDownCamera
import arx.graphics.text.TBitmappedFont
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11


class WindowingGraphicsData extends TGraphicsData with TMutableAuxData with TWorldAuxData {
	var desktop : Widget = _

	var widgetQuads = new MultiMap[Widget, WQuad]

	var desktopSize = Vec2T(PositionExpression.Constant(1440), PositionExpression.Constant(900))

	var pov = new PixelPOV
	val vbo = new AVBO(WindowingSystemAttributeProfile)
	val textureBlock = new TextureBlock(2048, 2048)
	textureBlock.minFilter = GL11.GL_NEAREST
	textureBlock.magFilter = GL11.GL_NEAREST

	var defaultBackgroundImage = ResourceManager.image("ui/styledBorder_wood_ne.png")
	private val _defaultDefaultFont = Executor.submitAsync(() => ResourceManager.font("pf_ronda_seven", textureBlock))
	var _defaultFont : Option[TBitmappedFont] = None
	def defaultFont_=(fontName : String): Unit = {
		_defaultFont = Some(ResourceManager.font(fontName, textureBlock))
	}
	def defaultFont = _defaultFont.getOrElse(_defaultDefaultFont.get())
}

class PixelPOV extends TCamera {
	override def modelviewMatrix(viewport : Recti): ReadMat4x4 = Mat4x4.Identity

	override def projectionMatrix(viewport: Recti): ReadMat4x4 =
		GL.ortho(0.0f,viewport.width,viewport.height,0.0f,-100.0f,100.0f)

	override def eye: ReadVec3f = Vec3f.Zero

	override def forward: ReadVec3f = Vec3f.UnitZ

	override def ortho: ReadVec3f = Vec3f.UnitX

	override def up: ReadVec3f = Vec3f.UnitY

	override def update(dt: UnitOfTime): Unit = {}

	override def moveEyeTo(eye: ReadVec3f): Unit = {}

	override def setMoveSpeed(multiplier: ReadVec3f): Unit = {}

	override def keymapNamespace: String = "PixelPOV"
}