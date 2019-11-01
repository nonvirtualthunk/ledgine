package arx.engine.game

import arx.core.units.UnitOfTime
import arx.engine.EnginePiece
import arx.engine.data.TimeData
import arx.engine.event.GameEvent
import arx.engine.game.components.GameComponent
import arx.engine.world.{Universe, World}

class GameEngine(var realtime : Boolean, universe : Universe, val world : World)
	extends EnginePiece[GameEngine, GameComponent, GameEvent](universe) {

	world.register[TimeData]
	world.attachWorldData(new TimeData)

	world.onEventCallbacks ::= ((world, e) => this.eventBus.fireEvent(e))

	override def currentTime(): UnitOfTime = {
		if (realtime) {
			super.currentTime()
		} else {
			world.view.worldData[TimeData].time
		}
	}
}

object GameEngine {
	val DefaultWorldKey = "Default Game World"
}