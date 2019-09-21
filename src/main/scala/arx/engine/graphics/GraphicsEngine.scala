package arx.engine.graphics

import arx.engine.EnginePiece
import arx.engine.game.GameEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.world.{Universe, World}

class GraphicsEngine(val gameEngine : GameEngine, universe : Universe, val displayWorld : World) extends EnginePiece[GraphicsEngine, GraphicsComponent](universe) {

		def draw(): Unit = {
			components = components.sortBy(_.drawPriority)
			components.foreach(_.draw(this))
		}
}
