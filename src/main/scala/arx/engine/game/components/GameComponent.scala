package arx.engine.game.components

import arx.core.units.UnitOfTime
import arx.engine.event.{DeferredInitializationEventBusListener, DeferredInitializationEventBusSender, GameEvent}
import arx.engine.game.GameEngine
import arx.engine.traits.EngineComponent
import arx.engine.world.EventState.Ended
import arx.engine.world.World

abstract class GameComponent extends EngineComponent[GameEngine] {
	private val gameEvents = new DeferredInitializationEventBusListener[GameEvent](true)

	override protected[engine] final def internalOnInitialize(engine: GameEngine): Unit = {
		gameEvents.initialize(engine.eventBus)
		listeners = List(gameEvents.eventBusListener)
	}

	def onGameEventEnd(listener: PartialFunction[GameEvent,_]): Unit = {
		gameEvents.onEvent(0) {
			case ge : GameEvent if ge.state == Ended && listener.isDefinedAt(ge) => listener(ge)
		}
	}

	def onGameEventEndWithPrecedence(precedence : Int)(listener: PartialFunction[GameEvent,_]): Unit = {
		gameEvents.onEvent(precedence) {
			case ge : GameEvent if ge.state == Ended && listener.isDefinedAt(ge) => listener(ge)
		}
	}

	override protected final def onUpdate(engine: GameEngine, dt: UnitOfTime): Unit = {
		onUpdate(engine.world, dt)
	}

	protected def onUpdate(world : World, dt : UnitOfTime) : Unit

	override protected def onInitialize(engine: GameEngine): Unit = {
		onInitialize(engine.world)
	}

	protected def onInitialize(world : World) : Unit
}
