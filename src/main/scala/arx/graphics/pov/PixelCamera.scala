package arx.graphics.pov

import arx.core.mat.{Mat4x4, ReadMat4x4}
import arx.core.math.Recti
import arx.core.units.UnitOfTime
import arx.core.vec.{ReadVec3f, Vec2f, Vec2i, Vec3f}
import arx.graphics.GL
import org.lwjgl.glfw.GLFW
import arx.Prelude._
import arx.application.Noto
import arx.engine.control.event.Keymap

import scala.language.postfixOps

class PixelCamera(pixelsPerSecond : Int, secondsToFullSpeed : Float) extends TCamera {
	import PixelCamera._

	var offset = Vec2f(0,0)
	val offsetVelocity = Vec2f(0,0)
	val offsetAcceleration = 1.0f / secondsToFullSpeed
	val offsetTargetVelocity = Vec2f(0,0)
	var moveSpeedMultiplier : Float = pixelsPerSecond
	var lastUpdatedTime = GLFW.glfwGetTime()

	override def modelviewMatrix(viewport: Recti): ReadMat4x4 = {
		GL.translate(Mat4x4.Identity, offset.x.round + viewport.width / 2, offset.y.round + viewport.height / 2, 0.0f)
	}

	override def projectionMatrix(viewport: Recti): ReadMat4x4 = {
		GL.ortho(0,viewport.width,0,viewport.height,-100,100)
	}

	override def eye: ReadVec3f = Vec3f(offset.x.round,offset.y.round,0)

	override def forward: ReadVec3f = Vec3f(0,0,-1)

	override def ortho: ReadVec3f = Vec3f.UnitX

	override def up: ReadVec3f = Vec3f.UnitY

	override def update(): Unit = {
		manualUpdate()
	}

	override def look(): Unit = {
		super.look()
	}

	def manualUpdate(): Unit = {
		val rawTime = (GLFW.glfwGetTime() * 1e6).toInt // microseconds
		val curTime = (((rawTime / 16666)+1) * 16666).toFloat / 1e6.toFloat
//		val curTime = rawTime.toFloat / 1e6
//		val dt = (curTime - lastUpdatedTime).toFloat // in seconds
//
//		offset += Vec2f(0.0f,5f * (dt / 0.0166667f))
//		lastUpdatedTime = curTime

//		val curTime = GLFW.glfwGetTime()
		val dt = (curTime - lastUpdatedTime).toFloat // in seconds

		if (dt > 0.001f) {
			offsetTargetVelocity.x = deltaFromMappings(MoveRight, MoveLeft, 1.0f)
			offsetTargetVelocity.y = deltaFromMappings(MoveUp, MoveDown, 1.0f)

			offset += offsetVelocity * moveSpeedMultiplier * dt
			for (axis <- 0 until 2) {
				if (offsetVelocity(axis) != offsetTargetVelocity(axis)) {
					val targetDv = offsetTargetVelocity(axis) - offsetVelocity(axis)
					val normTargetDv = signN0(targetDv)
					val magDv = targetDv.abs
					if (magDv < offsetAcceleration * dt) {
						offsetVelocity(axis) = offsetTargetVelocity(axis)
					} else {
						offsetVelocity(axis) += normTargetDv * offsetAcceleration * dt
					}
				}
			}
			lastUpdatedTime = curTime
		}
	}

	override def moveEyeTo(eye: ReadVec3f): Unit = {
		offset.x = eye.x.round
		offset.y = eye.y.round
	}

	override def setMoveSpeed(multiplier: ReadVec3f): Unit = {
		moveSpeedMultiplier = multiplier.x
	}

	override def keymapNamespace: String = PixelCamera.namespace

	TCamera.cameras ::= this
}


object PixelCamera {
	val MoveLeft = "moveLeft"
	val MoveRight = "moveRight"
	val MoveUp = "moveForward"
	val MoveDown = "moveBack"

	val namespace = "PixelCamera"

	Keymap.register(namespace, MoveLeft, GLFW.GLFW_KEY_LEFT)
	Keymap.register(namespace, MoveRight, GLFW.GLFW_KEY_RIGHT)
	Keymap.register(namespace, MoveUp, GLFW.GLFW_KEY_UP)
	Keymap.register(namespace, MoveDown, GLFW.GLFW_KEY_DOWN)
}