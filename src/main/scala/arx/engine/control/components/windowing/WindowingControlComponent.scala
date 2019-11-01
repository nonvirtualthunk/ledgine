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
import arx.engine.control.event.{KeyPressEvent, KeyReleaseEvent}
import arx.resource.ResourceManager
import org.lwjgl.glfw.GLFW

class WindowingControlComponent extends ControlComponent {
	var windowingSystem : WindowingSystem = _


	override protected def onUpdate(game: World, display: World, dt: UnitOfTime): Unit = {
		windowingSystem.update()


	}

	override protected def onInitialize(game: World, display: World): Unit = {
		display.onDataAddedCallbacks ::= ((world, entity, data) => {
			data pmatch {
				case _ : WidgetData => // do nothing
				case otherWAD : TWidgetAuxData => {
					val wd = display.data[WidgetData](entity)
					val watcher = Watcher(otherWAD.modificationSignature)
					wd.modificationWatchers ::= watcher
				}
			}
		})

		windowingSystem = new WindowingSystem(display, func => onControlEvent(func))
		val WD = display.worldData[WindowingGraphicsData]
		WD.desktop.x = PositionExpression.Constant(0)
		WD.desktop.y = PositionExpression.Constant(0)
		WD.desktop.z = PositionExpression.Constant(0)

		WD.desktop.width = DimensionExpression.Constant(GL.viewport.width)
		WD.desktop.height = DimensionExpression.Constant(GL.viewport.height)

		onControlEvent {
			case KeyReleaseEvent(key, modifiers) if key == GLFW.GLFW_KEY_R && modifiers.ctrl => {
				if (ResourceManager.useLocalResources) {
					ResourceManager.refreshSML()
					display[WindowingControlData].desktop.windowingSystem.reloadWidgets()
				}
			}
		}
	}
}
