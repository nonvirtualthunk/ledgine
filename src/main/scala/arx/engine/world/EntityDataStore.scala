package arx.engine.world

import arx.core.introspection.{CopyAssistant, ReflectionAssistant}
import arx.engine.entity.Entity
import overlock.atomicmap.AtomicMap

class EntityDataStore[T](val clazz : Class[T]) {
	val values = AtomicMap.atomicNBHM[Long, EntityDataWrapper[T]]
	val overlay = AtomicMap.atomicNBHM[Long, T]
	var hasOverlay = false
	val sentinel = ReflectionAssistant.instantiate(clazz)

	def entities = values.keys.view.map(k => new Entity(k))

	protected[engine] def putUntyped(entity : Entity, data : AnyRef, time : GameEventClock) : Unit = {
		put(entity, data.asInstanceOf[T], time)
	}

	def put(entity : Entity, data : T, time : GameEventClock) : Unit = {
		values.put(entity.id, new EntityDataWrapper[T](data, time))
	}

	protected[engine] def putOverlayUntyped(entity : Entity, data : AnyRef) : Unit = {
		hasOverlay = true
		overlay.put(entity.id, data.asInstanceOf[T])
	}

	def clearOverlay(): Unit = {
		overlay.clear()
		hasOverlay = false
	}

	def getOpt(entity : Entity) : Option[T] = {
		if (hasOverlay) {
			overlay.get(entity.id).orElse(values.get(entity.id).map(v => v.data))
		} else {
			values.get(entity.id).map(v => v.data)
		}
	}

	def getOrElseUpdate(entity : Entity, time : GameEventClock) : T = {
		if (hasOverlay) {
			overlay.get(entity.id).getOrElse(values.getOrElseUpdate(entity.id, new EntityDataWrapper[T](ReflectionAssistant.instantiate(clazz), time)).data)
		} else {
			values.getOrElseUpdate(entity.id, new EntityDataWrapper[T](ReflectionAssistant.instantiate(clazz), time)).data
		}
	}

	def get(entity : Entity) : T = {
		if (hasOverlay) {
			overlay.get(entity.id).orElse(values.get(entity.id).map(v => v.data)).getOrElse(sentinel)
		} else {
			values.get(entity.id).map(v => v.data).getOrElse(sentinel)
		}
	}

	def getOverlayOpt(entity : Entity) : Option[T] = {
		if (hasOverlay) {
			overlay.get(entity.id)
		} else {
			None
		}
	}

	def contains(entity : Entity) : Boolean = {
		values.contains(entity.id) || (hasOverlay && overlay.contains(entity.id))
	}

	protected[engine] def getUntyped(entity : Entity) : AnyRef = {
		this.get(entity).asInstanceOf[AnyRef]
	}

	def copyForEntitiesAtTime(entityIdsSet : Set[Long], atTime : GameEventClock) : this.type = {
		val newDataStore = new EntityDataStore[T](clazz)

		values.foreach { case (entityId, value) => {
			if (entityIdsSet(entityId) && value.addedAt <= atTime) {
				newDataStore.values.put(entityId, CopyAssistant.copy(value))
			}
		}}

		newDataStore.asInstanceOf[this.type]
	}
}
