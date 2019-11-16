package arx.engine.world

import arx.application.Noto
import arx.core.introspection.{Clazz, CopyAssistant, Field}
import arx.core.metrics.Metrics
import arx.engine.data.{TAuxData, TMutableAuxData}
import arx.engine.entity.Entity
import arx.engine.event.GameEvent

import scala.reflect.ClassTag

class WorldView(val world : World) {
	protected[engine] var dataStores = Map[Class[_], EntityDataStore[_ <: TAuxData]]()
	protected[engine] var _events = Vector[EventWrapper]()
	protected[engine] var _entities = Vector[EntityWrapper]()
	protected[engine] var modifications = Vector[Modification]()

	protected[engine] var nextDataRegistrationsIndex = 0
	protected[engine] var nextToggleIndex = 0
	protected[engine] var selfEntity : Entity = Entity.Sentinel

	def nextTime = GameEventClock(wrappedEvents.size)
	def currentTime = GameEventClock(nextTime.time - 1)

	protected[engine] def wrappedEvents = _events
	def events : Iterable[GameEvent] = this._events.view.map(ew => ew.event)

	def entities : Iterable[Entity] = _entities.view.map(ew => ew.entity)

	protected[engine] def dataStoreForClassOpt(clazz : Class[_]) : Option[TEntityDataStore[_]] = {
		dataStores.get(clazz)
	}

	protected[engine] final def dataStoreForClass(clazz : Class[_]) : TEntityDataStore[_] = {
		dataStoreForClassOpt(clazz) match {
			case Some(store) => store
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${clazz} was not")
		}
	}

	def entitiesWithData[T <: TAuxData](implicit tag : ClassTag[T]) : Iterable[Entity] = {
		dataStore[T].entities
	}

	final def entitiesMatching[T <: TAuxData](predicate : T => Boolean)(implicit tag : ClassTag[T]) : Iterable[Entity] = {
		entitiesWithData[T].filter(e => predicate(this.data[T](e)))
	}

	@inline
	def dataOptByClass[T](entity : Entity, clazz : Class[T]) : Option[T] = {
		dataStores.get(clazz).flatMap(store => store.getOpt(entity)).asInstanceOf[Option[T]]
	}

	@inline
	def hasDataByClass[T <: TAuxData](entity : Entity, runtimeClass : Class[_]) : Boolean = {
		dataStores.get(runtimeClass) match {
			case Some(store) => store.contains(entity)
			case None => false
		}
	}

	def applyOverlayModification(modification : Modification) : Unit = {
		val dataStore = dataStores(modification.modifiedType)
		// get the existing overlay data if possible, and if not get a copy of the original
		val existingDataOpt = dataStore.getOverlayOpt(modification.entity)
		val data = existingDataOpt.getOrElse(CopyAssistant.copy(dataStore.get(modification.entity)))
		modification.modifier.applyUntyped(data)

		dataStore.putOverlayUntyped(modification.entity, data)
	}

	def clearOverlay(): Unit = {
		dataStores.values.foreach(ds => ds.clearOverlay())
	}

	protected[engine] def applyModification(modification : Modification): Unit = {
		val data = dataStores(modification.modifiedType).get(modification.entity)
		modification.modifier.applyUntyped(data)
	}

	protected[engine] def toggleModification(modificationRef : ModifierReference, rootValue : AnyRef, enable : Boolean) : Unit = {
		import arx.Prelude.toRichTimer
		WorldView.toggleTimer.timeStmt {
			val modification = resolveModification(modificationRef)
			modification.toggles :+= (currentTime -> enable)
			val entity = modification.entity
			recomputeData(entity, modificationRef.modifiedType, rootValue)
		}
	}

	protected[engine] def recomputeData(entity : Entity, dataType : Class[_], rootValue : AnyRef) : Unit = {
		val data = dataStores(dataType)
		// we want to copy all values _into_ the given data so that the reference remains valid
		Clazz.fromClassOpt(dataType) match {
			case Some(clazz) =>
				val entityData = data.get(entity)
				// reset the root value to the starting point provided
				clazz.copyIntoUntyped(rootValue, entityData)
				// re-apply all of the modifications that are relevant to the given entity/data type pair
				modifications.filter(m => m.entity == entity && m.modifiedType == dataType && m.isActiveAt(currentTime)).foreach(_.modifier.applyUntyped(entityData))
			case None => Noto.severeError("Cannot toggle modifications without a Clazz implementation")
		}
	}

	protected[engine] def resolveModification(modificationRef: ModifierReference) : Modification = {
		modifications(modificationRef.index)
	}

	protected[engine] def copyAtTime(atTime : GameEventClock) : WorldView = {
		val view = new WorldView(world)
		view.selfEntity = selfEntity
		view._events = this.wrappedEvents.filter(e => e.occurredAt <= atTime)
		view._entities = this._entities.filter(e => e.addedAt <= atTime)

		val entitySet = view.entities.map(e => e.id).toSet
		view.dataStores = this.dataStores.map { case (k,v) => {
			if (classOf[TMutableAuxData].isAssignableFrom(k)) {
				// mutable aux data shares references across all views, but views are _encouraged_ not to allow modification
				(k, v)
			} else {
				// non mutable data gets independent copies
				(k, v.copyForEntitiesAtTime(entitySet, atTime).asInstanceOf[EntityDataStore[_ <: TAuxData]])
			}
		} }.toMap

		val upToModificationIndex = view.wrappedEvents.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		view.modifications = this.modifications.take(upToModificationIndex)
		view.modifications.foreach(m => if (m.isActiveAt(atTime)) { view.applyModification(m) })

		view
	}

	def dataModificationLog[C <: TAuxData](entity : Entity, baseValue : C)(implicit tag : ClassTag[C]) = {
		val curValue = CopyAssistant.copy(baseValue)
		var breakdownByField = Map[Field[_,_], Vector[BreakdownElement[Any]]]()

		for (m <- modifications.filter(m => m.entity == entity && m.modifiedType == tag.runtimeClass && m.isActiveAt(currentTime))) {
			m.modifier match {
				case FieldOperationModifier(field, operation) =>
					val oldFieldValue = field.getValue(curValue)
					m.modifier.applyUntyped(curValue)
					val newFieldValue = field.getValue(curValue)

					val impact = Impact.fromBeforeAfterAny(oldFieldValue, newFieldValue).getOrElse(m.modifier.impact)

					val existingElements = breakdownByField.getOrElse(field, Vector())
					breakdownByField += field -> (existingElements :+ BreakdownElement(m.source, m.modifier.description, impact))
				case NestedKeyedModifier(topField, key, nestedModifier) =>
					m.modifier.applyUntyped(curValue)

					val existingElements = breakdownByField.getOrElse(topField, Vector())
					breakdownByField += topField -> (existingElements :+ BreakdownElement(m.source, s"[$key] + ${m.modifier.description}", nestedModifier.impact))
				case _ =>
					Noto.error("Non-field operations not fully supported in dataModificationLog")
					m.modifier.applyUntyped(curValue)
			}
		}

		val breakdowns : Map[Field[_,_], Breakdown[Any]] = breakdownByField.map { case (field, elements) => field -> Breakdown[Any](field.getValue(curValue), elements) }.toMap
		new DataModificationLog[C](baseValue, curValue, breakdowns)
	}

	def dataModificationLog[C <: TAuxData](entity : Entity)(implicit tag : ClassTag[C]) : DataModificationLog[C] = {
		world.dataModificationLog[C](entity)
	}

	def worldCached[X](compute : => X) = new WorldCachedValue(this, compute)


	// +================================ convenience methods ===========================================+

	final def dataStore[T <: TAuxData](implicit tag : ClassTag[T]) : TEntityDataStore[T] = {
		dataStoreForClass(tag.runtimeClass.asInstanceOf[Class[T]]).asInstanceOf[TEntityDataStore[T]]
	}

	final def data[T <: TAuxData](entity : Entity)(implicit tag : ClassTag[T]) : T = {
		dataByClass[T](entity, tag.runtimeClass.asInstanceOf[Class[T]])
	}

	final def data[T <: TAuxData](clazz : Clazz[T])(entity : Entity) : T = {
		dataByClass[T](entity, clazz.runtimeClass)
	}


	final def worldData[T <: TAuxData](implicit tag : ClassTag[T]) : T = {
		data[T](selfEntity)(tag)
	}

	final def dataOpt[T <: TAuxData](entity : Entity, clazz : Clazz[T]) : Option[T] = {
		dataOptByClass(entity, clazz.runtimeClass)
	}

	final def dataOpt[T <: TAuxData](entity : Entity)(implicit tag : ClassTag[T]) : Option[T] = {
		dataOptByClass[T](entity, tag.runtimeClass.asInstanceOf[Class[T]])
	}

	final def hasData[T <: TAuxData](entity : Entity)(implicit tag : ClassTag[T]) : Boolean = {
		hasDataByClass[T](entity, tag.runtimeClass)
	}

	@inline
	def dataByClass[T <: TAuxData](entity : Entity, clazz : Class[T]) : T = {
		dataOptByClass[T](entity, clazz) match {
			case Some(data) => data
			case None => throw new IllegalStateException(s"Types must be registered with an entity and world before use, ${clazz} was not")
		}
	}
}

object WorldView {
	val toggleTimer = Metrics.timer("WorldView.toggleModification")
}