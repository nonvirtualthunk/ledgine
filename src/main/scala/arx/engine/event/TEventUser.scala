package arx.engine.event

import arx.Prelude
import arx.core.NoAutoLoad
import arx.engine.data.Moddable

import scala.collection.mutable

trait TEventUser {
	import TEventUser.Listener
	@NoAutoLoad @transient var _eventListeners : List[Listener] = Nil
	def eventListeners : List[Listener] = if ( _eventListeners != null ) { _eventListeners } else { Nil }
	def eventListeners_= ( e : List[Listener] ) { _eventListeners = e }

	@NoAutoLoad @transient var _successors : List[Moddable[TEventUser]] = Nil
	def successors : List[Moddable[TEventUser]] = if (_successors != null) { _successors } else { Nil }
	def successors_= (e : List[Moddable[TEventUser]]) { _successors = e }

	def onEvent ( func: PartialFunction[Event,_] ){
		eventListeners ::= Listener(func,consume = false,0)
	}
	def consumeEvent ( func : PartialFunction[Event,_] ){
		eventListeners ::= Listener(func,consume = true,0)
	}
	def consumeEventHighPrecedence(func : PartialFunction[Event,_]): Unit = {
		eventListeners ::= Listener(func,consume = true,1)
	}
	def onEventFallback ( func : PartialFunction[Event,_]): Unit = {
		eventListeners ::= Listener(func,consume = false,-1)
	}

	/**
	 * Handle the given event and pass it through all attached listeners, and up to successors if necessary
	 * @return true if any listener, or any successor's listener, examined the event, though not necessarily consumed it
	 */
	def handleEvent ( event: Event ) : Boolean = {
		TEventUser.pushEvent(event)
		// Sort the event listeners in order of precedence
		if (_eventListeners != null) {
			_eventListeners = _eventListeners.sortBy(_.precedence * -1)
		}

		var processed = false

		for ( Listener(listener,consumeByDefault,_: Int) <- eventListeners ; if event.notConsumed ) {
			if ( listener.isDefinedAt(event) ){
				val ret = listener(event)
				val shouldConsume = ret match {
					case b : Boolean => b
					case u : Unit => consumeByDefault//ok, no return, that's fine
					case a => consumeByDefault
				}
				if ( shouldConsume && event.notConsumed ) {
					event.consume()
				} else {  }
				processed = true
			}
		}


		for (successor <- successors ; if event.notConsumed) {
			if (successor.resolve().handleEvent(event)) {
				processed = true
			}
		}

		TEventUser.popEvent(event)
		processed
	}

	protected def consume() { TEventUser.activeEvent.consume() }
	protected def activeEvent = TEventUser.activeEvent
}

trait TForwardingEventUser extends TEventUser {

}


object TEventUser {
	var eventStack = new ThreadLocal[mutable.Stack[Event]]{
		override def initialValue() = mutable.Stack[Event]()
	}

	def pushEvent (evt:Event) { eventStack.get.push(evt) }
	def popEvent (evt:Event) { val ret = eventStack.get.pop(); Prelude.posit(ret eq evt,"Event pop mismatch, " + ret + " , " + evt) }
	def activeEvent = eventStack.get.top

	case class Listener (func : PartialFunction[Event,_], consume : Boolean, precedence: Int)

	val DevNull = new TEventUser {
		override def eventListeners: List[Listener] = Nil
		override def successors: List[Moddable[TEventUser]] = Nil
	}
}