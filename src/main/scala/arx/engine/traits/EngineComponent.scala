package arx.engine.traits

/**
  * TODO: Add javadoc
  */

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import arx.Prelude._
import arx.application.Noto
import arx.core.TDependable
import arx.core.units.UnitOfTime
import arx.engine.EnginePiece
import arx.engine.data.TWorldAuxData
import arx.engine.event.EventBusListener
import arx.engine.world.World



abstract class EngineComponent[PieceType <: EnginePiece[_, _, _]] extends TDependable {
	val updateInProgress = new AtomicBoolean(false)
	val lastUpdated = new AtomicReference(0.seconds)
	var initialized = false
	var updateInterval = (1/60.0).seconds

	var listeners : List[EventBusListener[_]] = Nil

	final def update(engine : PieceType, dt : UnitOfTime): Unit = {
		if (!initialized) {
			Noto.error("Components are supposed to be initialized before they are updated")
			initialize(engine)
		}
		// process all the asynchronously queued events
		listeners.foreach(l => l.process())

		if (needsUpdate) {
			onUpdate(engine, dt)
		}
	}

	protected def onUpdate(engine : PieceType, dt : UnitOfTime): Unit = {

	}

	protected def needsUpdate = true

	protected[engine] final def initialize(engine : PieceType): Unit = {
		internalOnInitialize(engine)
		onInitialize(engine)
		initialized = true
	}

	protected def internalOnInitialize(engine : PieceType) : Unit

	protected def onInitialize(engine : PieceType): Unit = {

	}
}
