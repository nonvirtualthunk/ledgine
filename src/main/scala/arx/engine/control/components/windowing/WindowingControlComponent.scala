package arx.engine.control.components.windowing

import arx.core.units.UnitOfTime
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.control.components.windowing.widgets.{DimensionExpression, PositionExpression}
import arx.engine.graphics.data.WindowingGraphicsData
import arx.engine.world.World
import arx.graphics.GL
import arx.Prelude._
import arx.core.datastructures.Watcher
import arx.engine.control.data.WindowingControlData
import arx.engine.control.event.{KeyPressEvent, KeyReleaseEvent, WidgetDestroyedEvent}
import arx.resource.ResourceManager
import org.lwjgl.glfw.GLFW
import arx.Prelude._
import arx.engine.data.TAuxData
import arx.engine.entity.Entity
import arx.engine.graphics.event.WidgetDestroyedGraphicsEvent

class WindowingControlComponent extends ControlComponent {
	var windowingSystem : WindowingSystem = _


	override protected def onUpdate(game: World, display: World, dt: UnitOfTime): Unit = {
//		windowingSystem.update()


	}

	def dataAddedCallback(display : World)(world : World, entity : Entity, data : TAuxData): Unit = {
		data match {
			case _ : WidgetData => // do nothing
			case otherWAD : TWidgetAuxData =>
				val wd = display.data[WidgetData](entity)
				if (otherWAD.hasModificationSignature) {
					val watcher = Watcher(otherWAD.modificationSignature)
					wd.modificationWatchers ::= watcher
				}
			case _ => // do nothing
		}
	}

	override protected def onInitialize(game: World, display: World): Unit = {
		display.onDataAddedCallbacks ::= dataAddedCallback(display)

		windowingSystem = new WindowingSystem(display, func => onControlEventWithPrecedence(1)(func), event => fireEvent(event))
		val WD = display.worldData[WindowingGraphicsData]
		WD.desktop.x = PositionExpression.Constant(0)
		WD.desktop.y = PositionExpression.Constant(0)

		WD.desktop.width = DimensionExpression.Constant(GL.viewport.width)
		WD.desktop.height = DimensionExpression.Constant(GL.viewport.height)

		onControlEventWithPrecedence(0) {
			case KeyReleaseEvent(key, modifiers) if key == GLFW.GLFW_KEY_R && modifiers.ctrl => {
				if (ResourceManager.useLocalResources) {
					ResourceManager.refreshSML()
					display[WindowingControlData].desktop.windowingSystem.reloadWidgets()
				}
			}

			case WidgetDestroyedEvent(widget) =>
				fireGraphicsEvent(WidgetDestroyedGraphicsEvent(widget))
		}
	}
}
