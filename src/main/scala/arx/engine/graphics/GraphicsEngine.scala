package arx.engine.graphics

import arx.Prelude
import arx.application.Noto
import arx.core.metrics.Metrics
import arx.engine.EnginePiece
import arx.engine.game.GameEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.graphics.event.GraphicsEvent
import arx.engine.world.{DebugWorld, Universe, World, WorldQuery}

class GraphicsEngine(val gameEngine : GameEngine, universe : Universe, val displayWorld : World)
	extends EnginePiece[GraphicsEngine, GraphicsComponent, GraphicsEvent](universe) {

	DebugWorld.displayWorld = displayWorld

	var first = true

	def draw(): Unit = {
		components = components.sortBy(_.drawPriority)
		components.foreach(_.draw(this))

		if (first) {
			Metrics.checkpoint("first frame drawn")
			first = false
		}
	}
}
