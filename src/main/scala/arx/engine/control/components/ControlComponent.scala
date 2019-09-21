package arx.engine.control.components

import arx.core.units.UnitOfTime
import arx.engine.control.ControlEngine
import arx.engine.event.{DeferredInitializationEventBusListener, Event}
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.DrawPriority
import arx.engine.traits.EngineComponent
import arx.engine.world.World

abstract class ControlComponent extends EngineComponent[ControlEngine] {
	private val gameEvents = new DeferredInitializationEventBusListener
	private val graphicsEvents = new DeferredInitializationEventBusListener
	private val controlEvents = new DeferredInitializationEventBusListener

	override protected[engine] final def internalOnInitialize(engine: ControlEngine): Unit = {
		gameEvents.initialize(engine.gameEngine.eventBus)
		graphicsEvents.initialize(engine.graphicsEngine.eventBus)
		controlEvents.initialize(engine.eventBus)
		listeners = List(gameEvents.eventBusListener, graphicsEvents.eventBusListener, controlEvents.eventBusListener)
	}

	def onGameEvent(listener: PartialFunction[Event,_]): Unit = {
		gameEvents.onEvent(listener)
	}

	def onGraphicsEvent(listener : PartialFunction[Event,_]) : Unit = {
		graphicsEvents.onEvent(listener)
	}

	def onControlEvent(listener : PartialFunction[Event,_]) : Unit = {
		controlEvents.onEvent(listener)
	}


	override protected final def onUpdate(controlEngine: ControlEngine, dt: UnitOfTime): Unit = {
		onUpdate(controlEngine.gameEngine.world, controlEngine.displayWorld, dt)
	}

	protected def onUpdate(game : World, graphics : World, dt : UnitOfTime) : Unit

	override protected def onInitialize(controlEngine: ControlEngine): Unit = {
		onInitialize(controlEngine.gameEngine.world, controlEngine.displayWorld)
	}

	protected def onInitialize(game : World, display : World) : Unit
}
