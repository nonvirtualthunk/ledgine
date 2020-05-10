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

class EventBus[T <: Event] extends TSentinelable {
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

	protected[event]var syncListeners : Vector[EventBusListener[T]] = Vector()

	def fireEvent(event : T): Unit = {
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

	def createListener() = new EventBusListener[T](this)

	def onEvent(listener: PartialFunction[Event,_]): Unit = {
		synchronousListener.onEvent(listener)
	}

	protected[event] val synchronousListener = createListener()
}

object EventBus {
	def Sentinel[T <: Event] : EventBus[T] = new EventBus[T] with TSentinel
}

class EventBusListener[T <: Event](val bus : EventBus[T]) extends TSentinelable {
	var lastReadOffset = 0L
	var cursor = 0
	var listeners = List[Listener[T]]()
	var active = true

	bus.lock.readLock {
		lastReadOffset = bus.minimumOffset
		cursor = bus.offsets.indexOf(lastReadOffset)
	}

	def process(): Unit = {
		if (active) {
			var toProcess = Vector[T]()
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
					toProcess :+= event.asInstanceOf[T]
					lastReadOffset = bus.offsets(cursor)
					cursor = (cursor + 1) & bus.capAND
				}
			}
			toProcess.foreach(e => {
				val relevantListeners = listeners.filter(l => l.func.isDefinedAt(e)).sortBy(_.precedence * -1)
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

	def onEvent (listener: PartialFunction[T,_]) : Listener[T] = {
		val newListener = Listener(listener, processConsumed = false)
		listeners ::= newListener
		newListener
	}

	def onEventWithPrecedence (precedence : Int)(listener: PartialFunction[T,_]) : Listener[T] = {
		val newListener = Listener(listener, processConsumed = false, precedence = precedence)
		listeners ::= newListener
		newListener
	}

	def listen (listener: PartialFunction[T,_]) = {
		onEvent(listener)
	}
}

object EventBusListener {
	case class Listener[T](func : PartialFunction[T,_], processConsumed : Boolean = false, var active : Boolean = true, var precedence : Int = 0) {
		def activate() { active = true }
		def deactivate() { active = false }
	}

	def Sentinel[T <: Event] : EventBusListener[T] = new EventBusListener[T](EventBus.Sentinel[T]) with TSentinel
}


/**
 * Wrapper around a conceptual listener to allow for listener functions to be registered before the actual event bus is known.
 * Useful for deferred initialization (as in the case of the engine components).
 */
class DeferredInitializationEventBusListener[T <: Event](sync : Boolean) {
	var eventBusListener = EventBusListener.Sentinel[T]
	var pendingListeners = List[EventBusListener.Listener[T]]()

	def onEvent(precedence : Int)(func : PartialFunction[T, _]) : Unit = {
		if (eventBusListener.isSentinel) {
			val l = Listener[T](func, processConsumed = false, active = true, precedence = precedence)
			pendingListeners ::= l
			l
		} else {
			eventBusListener.onEventWithPrecedence(precedence)(func)
		}
	}

	def initialize(bus : EventBus[T]): Unit = {
		if (sync) {
			eventBusListener = bus.synchronousListener
		} else {
			eventBusListener = bus.createListener()
		}
		eventBusListener.listeners :::= pendingListeners
		pendingListeners = Nil
	}
}

class DeferredInitializationEventBusSender[T <: Event] {
	var queuedEvents : Vector[T] = Vector()
	var eventBus : Option[EventBus[T]] = None

	def fireEvent(event : T): Unit = {
		eventBus match {
			case Some(eb) => eb.fireEvent(event)
			case None => queuedEvents :+= event
		}
	}

	def initialize(bus : EventBus[T]): Unit = {
		queuedEvents.foreach(bus.fireEvent)
		queuedEvents = Vector()
		eventBus = Some(bus)
	}
}