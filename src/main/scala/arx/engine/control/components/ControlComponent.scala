package arx.engine.control.components

import arx.core.units.UnitOfTime
import arx.engine.control.ControlEngine
import arx.engine.control.event.ControlEvent
import arx.engine.event.{DeferredInitializationEventBusListener, DeferredInitializationEventBusSender, Event, GameEvent}
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.DrawPriority
import arx.engine.graphics.event.GraphicsEvent
import arx.engine.traits.EngineComponent
import arx.engine.world.World

abstract class ControlComponent extends EngineComponent[ControlEngine] {
	private val gameEvents = new DeferredInitializationEventBusListener[GameEvent](false)
	private val graphicsEvents = new DeferredInitializationEventBusListener[GraphicsEvent](false)
	private val controlEvents = new DeferredInitializationEventBusListener[ControlEvent](true)

	private val controlEventSender = new DeferredInitializationEventBusSender[ControlEvent]

	override protected[engine] final def internalOnInitialize(engine: ControlEngine): Unit = {
		gameEvents.initialize(engine.gameEngine.eventBus)
		graphicsEvents.initialize(engine.graphicsEngine.eventBus)
		controlEvents.initialize(engine.eventBus)
		controlEventSender.initialize(engine.eventBus)
		listeners = List(gameEvents.eventBusListener, graphicsEvents.eventBusListener, controlEvents.eventBusListener)
	}

	def onGameEvent(listener: PartialFunction[Event,_]): Unit = {
		gameEvents.onEvent(0)(listener)
	}

	def onGraphicsEvent(listener : PartialFunction[Event,_]) : Unit = {
		graphicsEvents.onEvent(0)(listener)
	}

	def onControlEvent(listener : PartialFunction[Event,_]) : Unit = {
		controlEvents.onEvent(0)(listener)
	}
	def onControlEventWithPrecedence(precedence : Int)(listener : PartialFunction[Event,_]) : Unit = {
		controlEvents.onEvent(precedence)(listener)
	}

	def fireEvent(event : ControlEvent) : Unit = {
		controlEventSender.fireEvent(event)
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
