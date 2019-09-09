package arx.engine.world

import arx.application.Noto
import arx.core.introspection.{Clazz, CopyAssistant, Field}
import arx.engine.data.TAuxData

import scala.reflect.ClassTag

class WorldView(val world : World) {
	protected[engine] var dataStores = Map[Class[_], EntityDataStore[_ <: TAuxData]]()
	protected[engine] var _events = Vector[EventWrapper]()
	protected[engine] var entities = Vector[EntityWrapper]()
	protected[engine] var modifications = Vector[Modification]()

	protected[engine] var nextDataRegistrationsIndex = 0
	protected[engine] var selfEntity : Entity = Entity.Sentinel

	def nextTime = GameEventClock(events.size)
	def currentTime = GameEventClock(nextTime.time - 1)

	def events = this._events

	protected[engine] def dataStoreForClass(clazz : Class[_]) : EntityDataStore[_] = {
		dataStores.get(clazz) match {
			case Some(store) => store
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${clazz} was not")
		}
	}

	def dataStore[T <: TAuxData](implicit tag : ClassTag[T]) : EntityDataStore[T] = {
		dataStores.get(tag.runtimeClass) match {
			case Some(store) => store.asInstanceOf[EntityDataStore[T]]
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${tag.runtimeClass} was not")
		}
	}

	def data[T <: TAuxData](entity : Entity)(implicit tag : ClassTag[T]) : T = {
		dataStores.get(tag.runtimeClass) match {
			case Some(store) => store.get(entity).asInstanceOf[T]
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${tag.runtimeClass} was not")
		}
	}

	def data[T <: TAuxData](clazz : Clazz[T])(entity : Entity) : T = {
		dataStores.get(clazz.runtimeClass) match {
			case Some(store) => store.get(entity).asInstanceOf[T]
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${clazz.runtimeClass} was not")
		}
	}

	def worldData[T <: TAuxData](implicit tag : ClassTag[T]) : T = {
		data[T](selfEntity)(tag)
	}

	def dataOpt[T <: TAuxData](entity : Entity)(implicit tag : ClassTag[T]) : Option[T] = {
		dataStores.get(tag.runtimeClass).flatMap(store => store.getOpt(entity).asInstanceOf[Option[T]])
	}

	def dataOptByClass[T](entity : Entity, clazz : Class[T]) : Option[T] = {
		dataStores.get(clazz).flatMap(store => store.getOpt(entity)).asInstanceOf[Option[T]]
	}

	def hasData[T <: TAuxData](entity : Entity)(implicit tag : ClassTag[T]) : Boolean = {
		hasDataByClass[T](entity, tag.runtimeClass)
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

	protected[engine] def copyAtTime(atTime : GameEventClock) : WorldView = {
		val view = new WorldView(world)
		view.selfEntity = selfEntity
		view._events = this.events.filter(e => e.occurredAt <= atTime)
		view.entities = this.entities.filter(e => e.addedAt <= atTime)

		val entitySet = view.entities.map(e => e.entity.id).toSet
		view.dataStores = this.dataStores.map { case (k,v) => (k, v.copyForEntitiesAtTime(entitySet, atTime).asInstanceOf[EntityDataStore[_ <: TAuxData]]) }.toMap

		val upToModificationIndex = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		view.modifications = this.modifications.take(upToModificationIndex)
		view.modifications.foreach(m => view.applyModification(m))

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
}