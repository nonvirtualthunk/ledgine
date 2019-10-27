package arx.engine.graphics.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 12:07 PM
 */

import arx.core.units.UnitOfTime
import arx.engine.event.{DeferredInitializationEventBusListener, Event, GameEvent}
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.event.GraphicsEvent
import arx.engine.traits.EngineComponent
import arx.engine.world.World

abstract class GraphicsComponent extends EngineComponent[GraphicsEngine] {
	private val gameEvents = new DeferredInitializationEventBusListener[GameEvent](false)
	private val graphicsEvents = new DeferredInitializationEventBusListener[GraphicsEvent](false)

	override protected[engine] final def internalOnInitialize(engine: GraphicsEngine): Unit = {
		gameEvents.initialize(engine.gameEngine.eventBus)
		graphicsEvents.initialize(engine.eventBus)
		listeners = List(gameEvents.eventBusListener, graphicsEvents.eventBusListener)
	}

	def onGameEvent(listener: PartialFunction[Event,_]): Unit = {
		gameEvents.onEvent(listener)
	}

	def onGraphicsEvent(listener : PartialFunction[Event,_]) : Unit = {
		graphicsEvents.onEvent(listener)
	}


	override protected final def onUpdate(graphicsEngine: GraphicsEngine, dt: UnitOfTime): Unit = {
		onUpdate(graphicsEngine.gameEngine.world, graphicsEngine.displayWorld, dt, graphicsEngine.currentTime())
	}

	protected def onInitialize(game : World, display : World) : Unit

	protected def onUpdate(game: World, display: World, dt: UnitOfTime, time: UnitOfTime): Unit

	override protected def onInitialize(graphicsEngine: GraphicsEngine): Unit = {
		onInitialize(graphicsEngine.gameEngine.world, graphicsEngine.displayWorld)
	}



	final def draw (graphicsEngine : GraphicsEngine): Unit = {
		draw(graphicsEngine.gameEngine.world, graphicsEngine.displayWorld)
	}

	def draw(game : World, graphics : World)

	def drawPriority = DrawPriority.Standard
}

sealed case class DrawPriority(orderNumber : Int) extends Ordered[DrawPriority] {
	override def compare(that: DrawPriority): Int = orderNumber.compare(that.orderNumber)
}

object DrawPriority {
	val First = DrawPriority(-500)
	val Early = DrawPriority(-100)
	val Standard = DrawPriority(0)
	val Late = DrawPriority(100)
	val Final = DrawPriority(200)
}