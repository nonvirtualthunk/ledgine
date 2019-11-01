package arx.engine.world

import arx.core.introspection.{CopyAssistant, ReflectionAssistant}
import arx.engine.entity.Entity
import overlock.atomicmap.AtomicMap


trait TEntityDataStore[T] {
	def entities : Iterable[Entity]

	def getOpt(entity : Entity) : Option[T]

	def get(entity : Entity) : T

	def getRaw(entity : Entity) : Option[EntityDataWrapper[T]]

	def contains(entity : Entity) : Boolean
}

class UnionEntityDataStore[T](primary : TEntityDataStore[T], secondary : TEntityDataStore[T]) extends TEntityDataStore [T] {
	override def entities: Iterable[Entity] = primary.entities ++ secondary.entities

	override def getOpt(entity: Entity): Option[T] = primary.getOpt(entity).orElse(secondary.getOpt(entity))

	override def get(entity: Entity): T = primary.getOpt(entity).getOrElse(secondary.get(entity))

	override def getRaw(entity: Entity): Option[EntityDataWrapper[T]] = primary.getRaw(entity).orElse(secondary.getRaw(entity))

	override def contains(entity: Entity): Boolean = primary.contains(entity) || secondary.contains(entity)
}

class EntityDataStore[T](val clazz : Class[T]) extends TEntityDataStore[T] {
	val values = AtomicMap.atomicNBHM[Long, EntityDataWrapper[T]]
	val overlay = AtomicMap.atomicNBHM[Long, T]
	var hasOverlay = false
	val sentinel = {
		ReflectionAssistant.instantiateOpt(clazz) match {
			case Some(v) => v
			case None => ReflectionAssistant.companionFor(clazz) match {
				case Some(comp) if ReflectionAssistant.hasField(comp, "Sentinel") => {
					val s = ReflectionAssistant.getFieldValue(comp, "Sentinel")
					if (clazz.isAssignableFrom(s.getClass)) {
						s.asInstanceOf[T]
					} else {
						throw new IllegalArgumentException(s"Could not create entity data store of class ${clazz.getSimpleName}, had sentinel value of wrong type")
					}
				}
				case None => throw new IllegalArgumentException(s"Could not create entity data store of class ${clazz.getSimpleName}, could not create/identify sentinel value")
			}
		}
	}

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

	def getRaw(entity : Entity) : Option[EntityDataWrapper[T]] = {
		values.get(entity.id)
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

	def remove(entity : Entity) : Boolean = {
		values.remove(entity.id).isDefined
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
