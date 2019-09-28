package arx.engine.simple

import arx.core.units.UnitOfTime
import arx.core.vec.Cardinals
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.components.windowing.WindowingControlComponent
import arx.engine.control.components.windowing.widgets.PositionExpression.{Match, Relative}
import arx.engine.control.components.windowing.widgets.{ImageDisplayWidget, PositionExpression, TextDisplayWidget}
import arx.engine.control.data.WindowingControlData
import arx.engine.control.event.KeyPressEvent
import arx.engine.data.Moddable
import arx.engine.game.GameEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.windowing.WindowingGraphicsComponent
import arx.engine.graphics.data.WindowingGraphicsData
import arx.engine.graphics.data.windowing.ImageDisplay.{Center, Scale, ScaleToFit}
import arx.engine.world.{Universe, World}
import arx.engine.{Engine, Scenario}
import arx.graphics.TToImage
import arx.graphics.helpers.RichText
import arx.resource.ResourceManager
import org.lwjgl.glfw.GLFW

object WindowingSystemEngine extends Engine {

	class WindowingSystemDemoComponent(windowing : WindowingControlComponent) extends ControlComponent {

		override protected def onUpdate(game: World, graphics: World, dt: UnitOfTime): Unit = {}

		override protected def onInitialize(game: World, display: World): Unit = {
			val WD = display.worldData[WindowingControlData]

			val loaded = WD.desktop.createChild("DemoWidgets.DemoWidget")

			val text = WD.desktop.createChild(TextDisplayWidget)
			text.text = Moddable(RichText("Hello, world!"))
			text.y = Relative(loaded, 20, Cardinals.Down)
			text.x = Match(loaded)
			text.backgroundImage = Some(ResourceManager.image("ui/minimalistBorder_ne.png"))

			onControlEvent {
				case KeyPressEvent(key, modifiers, _) if key == GLFW.GLFW_KEY_R && modifiers.ctrl => {
					ResourceManager.refreshSML()
					WD.desktop.windowingSystem.reloadWidgets()
				}
			}
		}
	}


	override def onInit(): Unit = {
		loadScenario(new Scenario {
			override def gameWorld(universe: Universe): World = new World()

			override def displayWorld(universe: Universe): World = new World()
				.register[WindowingGraphicsData]()
				.register[WindowingControlData]

			override def realtime(universe: Universe): Boolean = true

			override def registerGameComponents(gameEngine: GameEngine, universe: Universe): Unit = {}

			override def registerGraphicsComponents(graphicsEngine: GraphicsEngine, universe: Universe): Unit = {
				graphicsEngine.register[WindowingGraphicsComponent]
			}

			override def registerControlComponents(controlEngine: ControlEngine, universe: Universe): Unit = {
				controlEngine.register[WindowingControlComponent]
				controlEngine.register[WindowingSystemDemoComponent]


			}
		}, new Universe)
	}
}
