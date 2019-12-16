package arx.engine.control.event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 8:01 AM
 */

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import arx.application.{Application, Noto}
import arx.core.traits.{ArxEnum, ArxEnumObject}
import arx.core.vec.{ReadVec2f, ReadVec2i}
import arx.engine.EngineCore
import arx.graphics.Image
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.{GLFW, GLFWImage}


object Mouse {
	def setVisible(visible: Boolean) = {
		desiredVisible.set(visible)
	}

	def setImage(img : Image, hotOffset : ReadVec2i): Unit = {
		desiredCursor.set(Some(img -> hotOffset))
	}
	def unsetImage() : Unit = {
		desiredCursor.set(None)
	}

	def reconcileCursor(window : Long) : Unit = {
		if (Application.isOpenGLThread()) {
			{
				val desired = desiredVisible.get()
				val visible = isVisible.get()
				if (visible != desired) {
					isVisible.set(desired)
					if (!desired) {
						GLFW.glfwSetInputMode(windowRef, GLFW.GLFW_CURSOR, GLFW.GLFW_CURSOR_HIDDEN)
					} else {
						GLFW.glfwSetInputMode(windowRef, GLFW.GLFW_CURSOR, GLFW.GLFW_CURSOR_NORMAL)
					}
				}
			}

			{
				val active = activeCursor
				val desired = desiredCursor.get()
				if (active != desired) {
					val toDelete = activeCursorHandle
					desired match {
						case Some((newImg, hotOffset)) =>
							val cursorImg = GLFWImage.create()
							cursorImg.width(newImg.width)
							cursorImg.height(newImg.height)
							val buff = BufferUtils.createByteBuffer(newImg.width * newImg.height * 4)
							for (y <- newImg.height - 1 to 0 by -1; x <- 0 until newImg.width) {
								buff.put(newImg.raw(x,y,0))
								buff.put(newImg.raw(x,y,1))
								buff.put(newImg.raw(x,y,2))
								buff.put(newImg.raw(x,y,3))
							}
							buff.flip()
							cursorImg.pixels(buff)

							val cursor = GLFW.glfwCreateCursor(cursorImg, hotOffset.x, hotOffset.y)
							GLFW.glfwSetCursor(window, cursor)
							activeCursorHandle = cursor
						case None =>
							val cursor = GLFW.glfwCreateStandardCursor(GLFW.GLFW_ARROW_CURSOR)
							GLFW.glfwSetCursor(window, cursor)
							activeCursorHandle = cursor
					}
					if (toDelete != -1) {
						GLFW.glfwDestroyCursor(toDelete)
					}
					activeCursor = desired
				}
			}
		} else {
			Noto.error("trying to reconcile mouse cursor from thread other than the opengl thread")
		}
	}

	private var activeCursorHandle = -1L

	var desiredCursor = new AtomicReference[Option[(Image, ReadVec2i)]](None)
	var activeCursor : Option[(Image, ReadVec2i)] = None

	var _currentPosition = new AtomicReference(ReadVec2f(0.0f,0.0f))
	var _previousPosition = new AtomicReference(ReadVec2f(0.0f,0.0f))
	var _buttonDown = Map(MouseButton.Left -> false, MouseButton.Right -> false, MouseButton.Middle -> false, MouseButton.Other -> false)
	var isVisible = new AtomicBoolean(true)
	var desiredVisible = new AtomicBoolean(true)
	var windowRef = 0L

	def isInWindow = currentPosition.x > 0 && currentPosition.y > 0 && currentPosition.x < EngineCore.windowWidth && currentPosition.y < EngineCore.windowHeight

	def currentPosition = _currentPosition.get()
	def currentPosition_=(p : ReadVec2f) { _currentPosition.set(p) }

	def previousPosition = _previousPosition.get()
	def previousPosition_=(p : ReadVec2f) { _previousPosition.set(p) }

	def buttonDown : Map[MouseButton,Boolean] = synchronized { _buttonDown }
	def buttonDown_=(m : Map[MouseButton, Boolean]) = synchronized { _buttonDown = m }

	def updatePosition(pos : ReadVec2f): Unit = {
		_previousPosition.set(currentPosition)
		currentPosition = pos
	}
}

class MouseButton(name : String) extends ArxEnum(name) {}

object MouseButton extends ArxEnumObject[MouseButton] {
	val Left = MouseButton("Left")
	val Right = MouseButton("Right")
	val Middle = MouseButton("Middle")
	val Other = MouseButton("Other")
}
