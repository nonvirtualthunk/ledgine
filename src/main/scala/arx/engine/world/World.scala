package arx.engine.world

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/13/18
  * Time: 7:30 AM
  */

import java.util.concurrent.atomic.AtomicLong

import arx.application.Noto
import arx.core.introspection._
import arx.engine.data.{TAuxData, TMutableAuxData, TWorldAuxData}
import arx.engine.entity.{Entity, IdentityData}
import arx.engine.event.GameEvent
import com.carrotsearch.hppc.LongLongOpenHashMap
import overlock.atomicmap.AtomicMap

import scala.reflect.ClassTag

case class GameEventClock(time : Int) extends AnyVal {
	def < (other : GameEventClock) : Boolean = this.time < other.time
	def > (other : GameEventClock) : Boolean = this.time > other.time
	def <= (other : GameEventClock) : Boolean = this.time <= other.time
	def >= (other : GameEventClock) : Boolean = this.time >= other.time
	def - (value : Int) : GameEventClock = GameEventClock(this.time - value)
	def + (value : Int) : GameEventClock = GameEventClock(this.time + value)

	def asInt = time
}


trait Modifier[T] {

	def apply(value : T) : Unit

	def applyUntyped(value : AnyRef): Unit = {
		this.apply(value.asInstanceOf[T])
	}

	def description : String

	def impact : Impact
}
case class LambdaModifier[T](function : T => Unit, description : String, impact : Impact) extends Modifier[T] {
	def apply(value : T) : Unit = {
		function(value)
	}
}
case class  FieldOperationModifier[C, T](field : Field[C,T], operation : Transformation[T]) extends Modifier[C] {
	def apply(dataObj : C): Unit = {
		val oldValue = field.getter(dataObj)
		val newValue = operation.apply(oldValue)
		field.setter(dataObj, newValue)
	}

	def description = operation.asSimpleString
	def impact = operation.impact
}
case class NestedModifier[C,T <: AnyRef,V](topField : Field[C,T], nestedOperation : Modifier[T]) extends Modifier[C]{
	override def apply(dataObject: C): Unit = {
		val nestedData = topField.getter(dataObject)
		val clonedData = CopyAssistant.copy(nestedData)
		nestedOperation.apply(clonedData)
		topField.setter(dataObject, clonedData)
	}

	def description = nestedOperation.description
	def impact = nestedOperation.impact
}
case class NestedKeyedModifier[C, K, V <: AnyRef, NV](topField : Field[C, Map[K, V]], key : K, nestedModifier : Modifier[V]) extends Modifier[C] {
	override def apply(dataObject: C): Unit = {
		val nestedMap = topField.getter(dataObject)
		nestedMap.get(key) match {
			case Some(nestedData) =>
				val clonedData = CopyAssistant.copy(nestedData)
				nestedModifier.apply(clonedData)
				val newMap = nestedMap + (key -> clonedData)
				topField.setter(dataObject, newMap)
			case None => Noto.warn(s"Modifier attempted to modify nested key $key, but that was not present in map, doing nothing")
		}
	}

	def description = nestedModifier.description
	def impact = nestedModifier.impact
}


case class Modification(modifiedType : Class[_ <: TAuxData], entity : Entity,modifier : Modifier[_],source : Option[String], var toggles : Vector[(GameEventClock, Boolean)] = Vector()) {
	def isActiveAt(time : GameEventClock) : Boolean = if (toggles.isEmpty) {
		true
	} else {
		toggles.takeWhile(t => t._1 <= time).lastOption.forall(t => t._2)
	}
}
object Modification {
	def apply[C <: TAuxData](entity : Entity, modifier : Modifier[C], source : Option[String])(implicit tag : ClassTag[C]) : Modification = {
		new Modification(tag.runtimeClass.asInstanceOf[Class[_ <: TAuxData]], entity, modifier, source)
	}
}
class ModifierReference(protected[engine] val index : Int, protected[engine] val modifiedType: Class[_]) {}

sealed abstract class EventState {}
object EventState {
	case object Started extends EventState {}
	case object Continues extends EventState {}
	case object Ended extends EventState {}
}

protected[engine] class EventWrapper(val event : GameEvent, val occurredAt : GameEventClock, val modificationIndexLimit : Int) {
	def state = event.state

	override def toString: String = s"EventWrapper($event, $occurredAt, $modificationIndexLimit)"
}
protected[engine] class EntityWrapper(val entity : Entity, val addedAt : GameEventClock)
protected[engine] class EntityDataWrapper[T](val data : T, val addedAt : GameEventClock)
protected[engine] case class EntityDataRegistration(entity : Entity, dataType : Class[_], atTime : GameEventClock)
protected[engine] case class ModifierToggle(modifierReference : ModifierReference, atTime : GameEventClock)

class World {
	protected[world] var entityCounter = new AtomicLong(0)
	protected[world] var dataRegistrations = Vector[EntityDataRegistration]()
	protected[world] var toggles = Vector[ModifierToggle]()
	protected[world] val foreignEntities = AtomicMap.atomicNBHM[Long,Long]

	var onEntityAddedCallbacks = List[(World, Entity) => Unit]()
	var onEntityRemovedCallbacks = List[(World, Entity) => Unit]()
	var onDataAddedCallbacks = List[(World, Entity, TAuxData) => Unit]()
	var onEventCallbacks = List[(World, GameEvent) => Unit]()

	protected[world] val coreView : WorldView = initializeCoreView()
	protected[world] val currentView : WorldView = initializeCurrentView()
	protected def initializeCoreView() : WorldView = new WorldView(this)
	protected def initializeCurrentView() : WorldView = new WorldView(this)

	protected[world] var selfEntity : Entity = createEntity()

	coreView.selfEntity = selfEntity
	currentView.selfEntity = selfEntity

	DebugWorld.world = this

	register[IdentityData]()

	def nextTime = coreView.nextTime
	def currentTime = coreView.currentTime

	// todo: rfind
	def eventAt(eventClock : GameEventClock) = coreView.wrappedEvents.find(e => e.occurredAt == eventClock)

	def register[T <: TAuxData]()(implicit tag : ClassTag[T]) : this.type = {
		registerClass(tag.runtimeClass.asInstanceOf[Class[T]])
	}

	def registerClass(clazz : Class[_ <: TAuxData]) : this.type = {
		val coreDataStore = new EntityDataStore(clazz)
		coreView.dataStores += clazz -> coreDataStore
		// mutable data shares references in all views, non-mutable data gets its own ledger based copy
		if (classOf[TMutableAuxData].isAssignableFrom(clazz)) {
			currentView.dataStores += clazz -> coreDataStore
		} else {
			currentView.dataStores += clazz -> new EntityDataStore(clazz)
		}
		this
	}

	def registerSubtypesOf[T <: TAuxData]()(implicit tag : ClassTag[T]) : Unit = {
		ReflectionAssistant.allSubTypesOf(tag.runtimeClass).foreach(clazz => registerClass(clazz.asInstanceOf[Class[_ <: TAuxData]]))
	}

	def createEntity(providedId : Long = -1) : Entity = {
		val newId = if (providedId == -1) { entityCounter.incrementAndGet() } else { providedId }
		val newEnt = new Entity(newId)
		coreView._entities :+= new EntityWrapper(newEnt, nextTime)
		currentView._entities = coreView._entities

		onEntityAddedCallbacks.foreach(c => c(this, newEnt))

		newEnt
	}

	def destroyEntity(entity : Entity) : Unit = {
		if (coreView.dataStores.exists(ds => !classOf[TMutableAuxData].isAssignableFrom(ds._1) && ds._2.contains(entity))) {
			Noto.error("Cannot destroy entities with non-mutable data types")
		} else {
			coreView.dataStores.foreach(ds => {
				ds._2.remove(entity)
			})
			coreView._entities = coreView._entities.filterNot(ew => ew.entity == entity)
			currentView._entities = coreView._entities
			// todo: maybe track _all_ created views and remove from all of them
			onEntityRemovedCallbacks.foreach(c => c(this, entity))
		}
	}

	/**
	 * Creates an entity representation of an entity from another world, will always return the same entity when given the same foreign entity
	 */
	def createForeignEntity(foreign : Entity) : Entity = {
		new Entity(foreignEntities.getOrElseUpdate(foreign.id, createEntity().id))
	}

	def data[T <: TMutableAuxData](entity : Entity)(implicit tag : ClassTag[T]) : T = {
		coreView.dataStores.get(tag.runtimeClass) match {
			case Some(store) =>
				val newCreation = !hasData[T](entity)
				val data = store.asInstanceOf[EntityDataStore[T]].getOrElseUpdate(entity, currentTime)
				if (newCreation) {
					onDataAddedCallbacks.foreach(cb => cb(this, entity, data))
				}
				data
			case None =>
				register[T]()
				data[T](entity)
		}
	}

	def allData(entity : Entity) : Iterable[_ <: TMutableAuxData] = {
		coreView.dataStores.values.flatMap(ds => {
			ds.getOpt(entity).filter(v => classOf[TMutableAuxData].isAssignableFrom(v.getClass))
		}).asInstanceOf[Iterable[_ <: TMutableAuxData]]
	}

	def dataOpt[T <: TMutableAuxData](entity : Entity)(implicit tag : ClassTag[T]) : Option[T] = {
		coreView.dataStores.get(tag.runtimeClass) match {
			case Some(store) =>
				store.asInstanceOf[EntityDataStore[T]].getOpt(entity)
			case None =>
				register[T]()
				dataOpt[T](entity)
		}
	}

	def hasData[T <: TMutableAuxData](entity : Entity)(implicit tag : ClassTag[T]) : Boolean = {
		coreView.hasData[T](entity)
	}

	def apply[T <: TMutableAuxData with TWorldAuxData](implicit tag : ClassTag[T]) : T = worldData[T]

	def worldData[T <: TMutableAuxData](implicit tag : ClassTag[T]) : T = {
		data[T](selfEntity)
	}

	final def attachData (entity : Entity) = {
		new AttachDataBuilder(this, entity)
	}

	final def attachData[T <: TAuxData](entity : Entity, data : T)(implicit tag : ClassTag[T]) : Unit = {
		attachDataByClass(entity, data, tag.runtimeClass.asInstanceOf[Class[_ <: TAuxData]])
	}

	def attachDataByClass(entity : Entity, data : TAuxData, runtimeClass : Class[_ <: TAuxData]) : Unit = {
		val viewsToAlter = if (classOf[TMutableAuxData].isAssignableFrom(runtimeClass)) {
			List(coreView)
		} else {
			List(coreView, currentView)
		}
		for (view <- viewsToAlter) {
			view.dataStores.get(runtimeClass) match {
				case Some(dataStore) => {
					val dataToProvide = if (!(view eq coreView)) {
						CopyAssistant.copy(data)
					} else {
						data
					}
					dataStore.putUntyped(entity, dataToProvide, currentTime)
				}
				case None => {
					registerClass(runtimeClass)
					attachDataByClass(entity, data, runtimeClass)
				}
			}
		}

		dataRegistrations :+= EntityDataRegistration(entity, runtimeClass, currentTime)
		onDataAddedCallbacks.foreach(cb => cb(this, entity, data))
	}

	final def attachDataWith[T <: TAuxData](entity : Entity, dataInit : T => Unit)(implicit tag : ClassTag[T]) : Unit = {
		val newData = ReflectionAssistant.instantiate(tag.runtimeClass.asInstanceOf[Class[T]])
		dataInit(newData)
		attachData(entity, newData)
	}

	final def attachWorldData[T <: TAuxData] (data : T)(implicit tag : ClassTag[T]) : Unit = {
		attachData[T](selfEntity, data)
	}

	def modify[T <: TAuxData](entity : Entity, modifier : Modifier[T], source : Option[String])(implicit tag : ClassTag[T]) : ModifierReference = {
		val index = coreView.modifications.size
		coreView.modifications :+= new Modification(tag.runtimeClass.asInstanceOf[Class[_ <: TAuxData]], entity, modifier, source)
		currentView.modifications = coreView.modifications

		currentView.applyModification(coreView.modifications.last)

		new ModifierReference(index, tag.runtimeClass)
	}

	final def modify[T <: TAuxData](entity : Entity, modifier : Modifier[T])(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(entity, modifier, None)
	}
	final def modify[T <: TAuxData](entity : Entity, modifier : Modifier[T], source : String)(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(entity, modifier, Some(source))
	}

	final def modifyWorld[T <: TAuxData](modifier : Modifier[T], source : Option[String])(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(selfEntity, modifier, source)
	}

	final def modifyWorld[T <: TAuxData](modifier : Modifier[T])(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(selfEntity, modifier, None)
	}

	def toggleModification(modifierReference: ModifierReference, enable : Boolean) : Unit = {
		val mod = coreView.resolveModification(modifierReference)
		val rootValue = coreView.dataByClass(mod.entity, mod.modifiedType)
		view.toggleModification(modifierReference, rootValue, enable)
		toggles :+= ModifierToggle(modifierReference, currentTime)
	}

	def view : WorldView = this.currentView

	def viewAtTime(time : GameEventClock): WorldView = {
		val newView = coreView.copyAtTime(time)
		newView.nextDataRegistrationsIndex = dataRegistrations.lastIndexWhere(r => r.atTime <= time) + 1
		newView.nextToggleIndex = toggles.lastIndexWhere(r => r.atTime <= time) + 1
		newView
	}

	/**
	  * Update the given view to the given time. That is, after updating the final event applied in
	  * the view will have the given time.
	  */
	def updateViewToTime(view : WorldView, time : GameEventClock) : Unit = {
		val newDataRegistrations = dataRegistrations.slice(view.nextDataRegistrationsIndex, dataRegistrations.size)
			.takeWhile(r => r.atTime <= time)

		newDataRegistrations
   		.foreach(registration => {
				val data = CopyAssistant.copy(coreView.dataStores(registration.dataType).getUntyped(registration.entity))
				view.dataStores(registration.dataType).putUntyped(registration.entity, data, registration.atTime)
			})

		view.nextDataRegistrationsIndex += newDataRegistrations.size

		val newToggles = toggles.slice(view.nextToggleIndex, toggles.size)
   		.takeWhile(r => r.atTime <= time)

		newToggles
			.foreach(toggle => {
				val mod = coreView.resolveModification(toggle.modifierReference)
				view.recomputeData(mod.entity, mod.modifiedType, coreView.dataByClass(mod.entity, mod.modifiedType))
			})


		val nextModificationStart = view.wrappedEvents.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		val newEvents = coreView.wrappedEvents.slice(view.nextTime.time, time.asInt+1) // +1 because we want to go up to and include the given time
		view._events ++= newEvents
		val nextModificationLimit = view.wrappedEvents.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)

		val newModifications = coreView.modifications.slice(nextModificationStart, nextModificationLimit)
		newModifications.foreach(mod => view.applyModification(mod))
		view.modifications ++= newModifications
	}

	protected[engine] def pushEvent(event : GameEvent, state : EventState): GameEventClock = {
		val time = nextTime
		event.world = this
		event.state = state
		val eventWrapper = new EventWrapper(event, time, coreView.modifications.size)
		coreView._events :+= eventWrapper
		currentView._events = coreView.wrappedEvents
		onEventCallbacks.foreach(cb => cb(this, event))
		time
	}

	final def eventStmt(event : => GameEvent)(stmt : => Unit): Unit = {
		startEvent(event)
		stmt
		endEvent(event)
	}
	final def startEvent(event : GameEvent): GameEventClock = { this.pushEvent(event, EventState.Started) }
	final def continueEvent(event : GameEvent) : GameEventClock = { this.pushEvent(event, EventState.Continues) }
	final def endEvent(event : GameEvent) : GameEventClock = { this.pushEvent(event, EventState.Ended) }
	final def addEvent(event : GameEvent) : GameEventClock = {
		this.pushEvent(event, EventState.Started)
		this.pushEvent(event, EventState.Ended)
	}

	def dataModificationLog[C <: TAuxData](entity : Entity)(implicit tag : ClassTag[C]) = {
		val rawData = coreView.data[C](entity)
		currentView.dataModificationLog[C](entity, rawData)
	}

	def isHypothetical = false

	def copyEntity(base: Entity): Entity = {
		val ent = createEntity()
		for (ds <- coreView.dataStores.values) {
			for (data <- ds.getOpt(base)) {
				attachData(ent, data.copy(this))
			}
		}
		ent
	}
}

class AttachDataBuilder(world : World, entity : Entity) {
	def ofType[T <: TAuxData](init : T => Unit)(implicit tag : ClassTag[T]) = {
		world.attachDataWith[T](entity,init)
		this
	}
}



class WorldCachedValue[T](worldView : WorldView, compute : => T) {
	var value : T = compute
	var lastResolved : GameEventClock = worldView.currentTime
	def resolve() : T = {
		if (worldView.currentTime > lastResolved) {
			lastResolved = worldView.currentTime
			value = compute
		}
		value
	}
}
object WorldCachedValue {
	implicit def autoResolve[T](wc : WorldCachedValue[T]) : T = wc.resolve()
}

