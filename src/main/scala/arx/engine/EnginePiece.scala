package arx.engine

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/19/15
  * Time: 5:13 PM
  */

import arx.Prelude
import arx.Prelude._
import arx.application.Noto
import arx.core.Dependency
import arx.core.async.KillableThread
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.engine.event.{Event, EventBus}
import arx.engine.traits.EngineComponent
import arx.engine.world.{Universe, World}


abstract class EnginePiece[T <: EnginePiece[T, Component, EventType], Component <: EngineComponent[T] : Manifest, EventType <: Event](val universe : Universe) {
	var parallelism = 3
	var components = List[Component]()
	protected var componentClasses = List[Class[_ <: Component]]()
	var updateThreads = List[KillableThread]()
	var serial = false
	val eventBus = new EventBus[EventType]

	protected var initialized = false

	def currentTime() = Prelude.curTime()

	def createUpdateThread(i: Int): KillableThread = {
		new KillableThread(KillableThread.GameLevel) {
			override def whileRunningDo(): Unit = {
				var anyUpdates = false

				for (component <- components) {
					val t = currentTime()
					val delta = t - component.lastUpdated.get()
					if (delta > component.updateInterval) {
						if (component.updateInProgress.compareAndSet(false, true)) {
							Metrics.timer(component.getClass.getSimpleName + ".updateDuration").timeStmt {
								updateComponent(component, delta)
							}
							component.lastUpdated.set(t)
							component.updateInProgress.set(false)
							anyUpdates = true
						}
					}
				}

				if (!anyUpdates) {
					// wait for 3 ms before having another look
//					LockSupport.parkNanos(3000000)
					try {
						Thread.sleep(3)
					} catch {
						case _: InterruptedException => this.kill()
					}
				}
			}
		}
	}

	def register[T <: Component : Manifest] = {
		if (!initialized) {
			componentClasses ::= manifest[T].runtimeClass.asInstanceOf[Class[T]]
		} else {
			Noto.severeError("Cannot add component after initialization")
		}
	}

	def update(deltaSeconds: Float, nSteps : Int = 1): Unit = {
		if (!initialized) {
			Noto.severeError("Attempted to update engine piece without initializing first")
		}
		if (serial) {
			for (n <- 0 until nSteps) {
				components.foreach(c => {
					Metrics.timer(c.getClass.getSimpleName + ".updateDuration").timeStmt {
						updateComponent(c,deltaSeconds.seconds)
					}
				})
			}
		}
	}

	def shutdown(): Unit = {
		updateThreads.foreach(t => t.kill())
	}

	/**
	 * Converts to serial mode for updates, killing all async update threads and making future calls to update(...) synchronous
	 */
	def serializeUpdates(): this.type = {
		serial = true
		updateThreads.foreach(t => t.kill())
		this
	}

	def resolveComponents(context : List[Any], onConstruction : Any => Unit) = {
		val (comps, newContext) = Dependency.resolveByInjection(componentClasses.reverse, this :: context, onConstruction)
		newContext
	}

	def updateComponent(component : Component, delta : UnitOfTime): Unit = {
		component.update(this.asInstanceOf[T],delta)
	}
	def initializeComponent(component: Component): Unit = {
		components :+= component
		component.initialize(this.asInstanceOf[T])
	}

	def initialize(serial: Boolean): Unit = {
		initialized = true
		this.serial = serial

		if (!serial) {
			updateThreads = fillList(parallelism)(i => createUpdateThread(i))
			updateThreads.foreach(t => t.start())
		}
	}
}
