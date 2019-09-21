package arx.engine.game.components

import arx.core.units.UnitOfTime
import arx.engine.event.{DeferredInitializationEventBusListener, Event, EventBusListener}
import arx.engine.game.GameEngine
import arx.engine.traits.EngineComponent
import arx.engine.world.World

abstract class GameComponent extends EngineComponent[GameEngine] {
	private val gameEvents = new DeferredInitializationEventBusListener

	override protected[engine] final def internalOnInitialize(engine: GameEngine): Unit = {
		gameEvents.initialize(engine.eventBus)
		listeners = List(gameEvents.eventBusListener)
	}

	def onGameEvent(listener: PartialFunction[Event,_]): Unit = {
		gameEvents.onEvent(listener)
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
