package arx.engine.event

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 2:34 PM
  */

import arx.application.Noto
import arx.core.synchronization.ReadWriteLock
import arx.core.traits.{TSentinel, TSentinelable}
import arx.engine.event.EventBusListener.Listener

class EventBus extends TSentinelable {
	protected[event] val sizePo2 = 11
	protected[event]val sizeLimit = 1 << sizePo2
	// bitmask that can be AND'd to a number to keep it within range of sizeLimit, effectively does % sizeLimit
	protected[event]val capAND = sizeLimit - 1
//	var events = new RingBuffer[Event](12) // hint the size to 4096 to start
	protected[event]val events = Array.ofDim[Event](sizeLimit)
	protected[event]val offsets = Array.fill(sizeLimit)(0L)
	protected[event]var absoluteCount = 0L
	protected[event]var minimumOffset = 0L

	protected[event]var cursor = 0
	protected[event]var lock = new ReadWriteLock

	def fireEvent(event : Event): Unit = {
		lock.writeLock {
			val overwrittenOffset = offsets(cursor)
			// keep track of the smallest offset present in the
			minimumOffset = math.max(overwrittenOffset, minimumOffset)

			events(cursor) = event
			offsets(cursor) = absoluteCount
			absoluteCount += 1
			cursor = (cursor + 1) & capAND

			synchronousListener.process()
		}
	}

	def createListener() = new EventBusListener(this)

	def onEvent(listener: PartialFunction[Event,_]): Unit = {
		synchronousListener.onEvent(listener)
	}

	protected[event] val synchronousListener = createListener()
}

object EventBus {
	val Sentinel : EventBus = new EventBus with TSentinel
}

class EventBusListener(val bus : EventBus) extends TSentinelable {
	var lastReadOffset = 0L
	var cursor = 0
	var listeners = List[Listener]()
	var active = true

	bus.lock.readLock {
		lastReadOffset = bus.minimumOffset
		cursor = bus.offsets.indexOf(lastReadOffset)
	}

	def process(): Unit = {
		if (active) {
			var toProcess = Vector[Event]()
			bus.lock.readLock {
				if (bus.minimumOffset > lastReadOffset) {
					Noto.warn("Resetting event bus listener to head, not reading fast enough")
					lastReadOffset = bus.minimumOffset - 1
					cursor = (bus.cursor + 1) & bus.capAND
					if (bus.offsets(cursor) != bus.minimumOffset) {
						Noto.error(s"Did not reset to head properly $cursor, ${bus.minimumOffset}, $lastReadOffset")
					}
				}
				while (cursor != bus.cursor) {
					val event = bus.events(cursor)
					if (event == null) {
						Noto.error(s"Somehow ended up with a null event in a bus listener: $cursor, $lastReadOffset")
					}
					toProcess +:= event
					lastReadOffset = bus.offsets(cursor)
					cursor = (cursor + 1) & bus.capAND
				}
			}
			toProcess.foreach(e => {
				val relevantListeners = listeners.filter(l => l.func.isDefinedAt(e))
				for (l <- relevantListeners) {
					if (e.notConsumed || l.processConsumed) {
						l.func.apply(e) match {
							case b: Boolean if b => e.consume()
							case _ => // do nothing
						}
					}
				}
			})
		}
	}

	def onEvent (listener: PartialFunction[Event,_]) : Listener = {
		val newListener = Listener(listener, processConsumed = false)
		listeners ::= newListener
		newListener
	}

	def listen (listener: PartialFunction[Event,_]) = {
		onEvent(listener)
	}
}

object EventBusListener {
	case class Listener(func : PartialFunction[Event,_], processConsumed : Boolean = false, var active : Boolean = true) {
		def activate() { active = true }
		def deactivate() { active = false }
	}

	val Sentinel : EventBusListener = new EventBusListener(EventBus.Sentinel) with TSentinel
}


/**
 * Wrapper around a conceptual listener to allow for listener functions to be registered before the actual event bus is known.
 * Useful for deferred initialization (as in the case of the engine components).
 */
class DeferredInitializationEventBusListener {
	var eventBusListener = EventBusListener.Sentinel
	var pendingListeners = List[EventBusListener.Listener]()

	def onEvent(func : PartialFunction[Event, _]) : Listener = {
		if (eventBusListener.isSentinel) {
			val l = Listener(func, processConsumed = false, active = true)
			pendingListeners ::= l
			l
		} else {
			eventBusListener.onEvent(func)
		}
	}

	def initialize(bus : EventBus): Unit = {
		eventBusListener = bus.createListener()
		eventBusListener.listeners :::= pendingListeners
		pendingListeners = Nil
	}
}