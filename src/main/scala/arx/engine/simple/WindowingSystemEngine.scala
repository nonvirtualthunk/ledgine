package arx.engine.simple

import arx.core.units.UnitOfTime
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.components.windowing.WindowingControlComponent
import arx.engine.control.components.windowing.widgets.{ImageDisplayWidget, PositionExpression}
import arx.engine.control.data.WindowingControlData
import arx.engine.game.GameEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.windowing.WindowingGraphicsComponent
import arx.engine.graphics.data.WindowingGraphicsData
import arx.engine.graphics.data.windowing.ImageDisplay.{ActualSize, Center, ScaleToFit}
import arx.engine.world.{Universe, World}
import arx.engine.{Engine, Scenario}
import arx.resource.ResourceManager

object WindowingSystemEngine extends Engine {

	class WindowingSystemDemoComponent(windowing : WindowingControlComponent) extends ControlComponent {

		override protected def onUpdate(game: World, graphics: World, dt: UnitOfTime): Unit = {}

		override protected def onInitialize(game: World, display: World): Unit = {
			val WD = display.worldData[WindowingControlData]
			val islandImage = WD.desktop.createChild(ImageDisplayWidget.build(ResourceManager.image("ui/islandImage.png"), Center, ActualSize(1.0f)))
			islandImage.x = PositionExpression.Centered
			islandImage.y = PositionExpression.Centered
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
