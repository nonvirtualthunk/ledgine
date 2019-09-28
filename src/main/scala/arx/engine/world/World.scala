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
import arx.engine.data.{TAuxData, TMutableAuxData}
import arx.engine.entity.Entity
import arx.engine.event.GameEvent

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
	val tag : ClassTag[T]

	def apply(value : T) : Unit

	def applyUntyped(value : AnyRef): Unit = {
		this.apply(value.asInstanceOf[T])
	}

	def description : String

	def impact : Impact
}
case class LambdaModifier[T](function : T => Unit, description : String, impact : Impact)(implicit val tag : ClassTag[T]) extends Modifier[T] {
	def apply(value : T) : Unit = {
		function(value)
	}
}
case class  FieldOperationModifier[C, T](field : Field[C,T], operation : Transformation[T])(implicit val tag : ClassTag[C]) extends Modifier[C] {
	def apply(dataObj : C): Unit = {
		val oldValue = field.getter(dataObj)
		val newValue = operation.apply(oldValue)
		field.setter(dataObj, newValue)
	}

	def description = operation.asSimpleString
	def impact = operation.impact
}
case class NestedModifier[C,T <: AnyRef,V](topField : Field[C,T], nestedOperation : Modifier[T])(implicit val tag : ClassTag[C]) extends Modifier[C]{
	override def apply(dataObject: C): Unit = {
		val nestedData = topField.getter(dataObject)
		val clonedData = CopyAssistant.copy(nestedData)
		nestedOperation.apply(clonedData)
		topField.setter(dataObject, clonedData)
	}

	def description = nestedOperation.description
	def impact = nestedOperation.impact
}
case class NestedKeyedModifier[C, K, V <: AnyRef, NV](topField : Field[C, Map[K, V]], key : K, nestedModifier : Modifier[V])(implicit val tag : ClassTag[C]) extends Modifier[C] {
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


class Modification(val modifiedType : Class[_], val entity : Entity, val modifier : Modifier[_], val source : Option[String], var toggles : List[(GameEventClock, Boolean)] = Nil) {
	def isActiveAt(time : GameEventClock) : Boolean = if (toggles.isEmpty) {
		true
	} else {
		toggles.takeWhile(t => t._1 <= time).lastOption.map(t => t._2).getOrElse(true)
	}
}
object Modification {
	def apply[C](entity : Entity, modifier : Modifier[C], source : Option[String])(implicit tag : ClassTag[C]) : Modification = {
		new Modification(tag.runtimeClass, entity, modifier, source)
	}
}
class ModifierReference(protected[engine] val index : Int) extends AnyVal {}

sealed abstract class EventState {}
object EventState {
	case object Started extends EventState {}
	case object Continues extends EventState {}
	case object Ended extends EventState {}
}

protected[engine] class EventWrapper(val event : GameEvent, val occurredAt : GameEventClock, val state : EventState, val modificationIndexLimit : Int)
protected[engine] class EntityWrapper(val entity : Entity, val addedAt : GameEventClock)
protected[engine] class EntityDataWrapper[T](val data : T, val addedAt : GameEventClock)
protected[engine] case class EntityDataRegistration(entity : Entity, dataType : Class[_], atTime : GameEventClock)

class World {
	protected var entityCounter = new AtomicLong(0)
	protected var dataRegistrations = Vector[EntityDataRegistration]()

	var onEntityAddedCallbacks = List[Entity => Unit]()
	var onEntityRemovedCallbacks = List[Entity => Unit]()
	var onDataAddedCallbacks = List[(Entity, TAuxData) => Unit]()

	protected val coreView : WorldView = new WorldView(this)
	protected val currentView : WorldView = new WorldView(this)

	protected val selfEntity : Entity = createEntity()

	coreView.selfEntity = selfEntity
	currentView.selfEntity = selfEntity

	DebugWorld.world = this

	def nextTime = coreView.nextTime
	def currentTime = coreView.currentTime

	// todo: rfind
	def eventAt(eventClock : GameEventClock) = coreView.events.find(e => e.occurredAt == eventClock)

	def register[T <: TAuxData]()(implicit tag : ClassTag[T]) : World = {
		val coreDataStore = new EntityDataStore[T](tag.runtimeClass.asInstanceOf[Class[T]])
		coreView.dataStores += tag.runtimeClass -> coreDataStore
		// mutable data shares references in all views, non-mutable data gets its own ledger based copy
		if (classOf[TMutableAuxData].isAssignableFrom(tag.runtimeClass)) {
			currentView.dataStores += tag.runtimeClass -> coreDataStore
		} else {
			currentView.dataStores += tag.runtimeClass -> new EntityDataStore[T](tag.runtimeClass.asInstanceOf[Class[T]])
		}
		this
	}

	def registerClass(clazz : Class[_ <: TAuxData]) : Unit = {
		coreView.dataStores += clazz -> new EntityDataStore(clazz)
		currentView.dataStores += clazz -> new EntityDataStore(clazz)
	}

	def registerSubtypesOf[T <: TAuxData]()(implicit tag : ClassTag[T]) : Unit = {
		ReflectionAssistant.allSubTypesOf(tag.runtimeClass).foreach(clazz => registerClass(clazz.asInstanceOf[Class[_ <: TAuxData]]))
	}

	def createEntity(providedId : Long = -1) : Entity = {
		val newId = if (providedId == -1) { entityCounter.incrementAndGet() } else { providedId }
		val newEnt = new Entity(newId)
		coreView.entities :+= new EntityWrapper(newEnt, nextTime)
		currentView.entities = coreView.entities

		onEntityAddedCallbacks.foreach(c => c(newEnt))

		newEnt
	}

	def data[T <: TMutableAuxData](entity : Entity)(implicit tag : ClassTag[T]) : T = {
		coreView.dataStores.get(tag.runtimeClass) match {
			case Some(store) =>
				val newCreation = !hasData[T](entity)
				val data = store.asInstanceOf[EntityDataStore[T]].getOrElseUpdate(entity, currentTime)
				if (newCreation) {
					onDataAddedCallbacks.foreach(cb => cb(entity, data))
				}
				data
			case None =>
				register[T]()
				data[T](entity)
		}
	}

	def allData(entity : Entity) : Iterable[_ <: TAuxData] = {
		coreView.dataStores.values.flatMap(ds => {
			ds.getOpt(entity)
		})
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

	def worldData[T <: TMutableAuxData](implicit tag : ClassTag[T]) : T = {
		data[T](selfEntity)
	}

	def attachData (entity : Entity) = {
		new AttachDataBuilder(this, entity)
	}

	def attachData[T <: TAuxData](entity : Entity, data : T)(implicit tag : ClassTag[T]) : Unit = {
		val viewsToAlter = if (classOf[TMutableAuxData].isAssignableFrom(tag.runtimeClass)) {
			List(coreView)
		} else {
			List(coreView, currentView)
		}
		for (view <- viewsToAlter) {
			view.dataStores.get(tag.runtimeClass) match {
				case Some(dataStore) => {
					val dataToProvide = if (!(view eq coreView)) {
						CopyAssistant.copy(data)
					} else {
						data
					}
					dataStore.putUntyped(entity, dataToProvide, currentTime)
				}
				case None => {
					register[T]()
					attachData[T](entity, data)
				}
			}
		}

		dataRegistrations :+= EntityDataRegistration(entity, tag.runtimeClass, currentTime)
		onDataAddedCallbacks.foreach(cb => cb(entity, data))
	}

	def attachDataWith[T <: TAuxData](entity : Entity, dataInit : T => Unit)(implicit tag : ClassTag[T]) : Unit = {
		val newData = ReflectionAssistant.instantiate(tag.runtimeClass.asInstanceOf[Class[T]])
		dataInit(newData)
		attachData(entity, newData)
	}

	def attachWorldData[T <: TAuxData] (data : T)(implicit tag : ClassTag[T]) : Unit = {
		attachData[T](selfEntity, data)
	}

	def modify[T](entity : Entity, modifier : Modifier[T], source : Option[String])(implicit tag : ClassTag[T]) : ModifierReference = {
		val index = coreView.modifications.size
		coreView.modifications :+= new Modification(tag.runtimeClass, entity, modifier, source)
		currentView.modifications = coreView.modifications

		currentView.applyModification(coreView.modifications.last)

		new ModifierReference(index)
	}

	def modify[T](entity : Entity, modifier : Modifier[T], source : String)(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(entity, modifier, Some(source))
	}

	def modifyWorld[T](modifier : Modifier[T], source : Option[String])(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(selfEntity, modifier, source)
	}

	def view : WorldView = this.currentView

	def viewAtTime(time : GameEventClock): WorldView = {
		val newView = coreView.copyAtTime(time)
		newView.nextDataRegistrationsIndex = dataRegistrations.lastIndexWhere(r => r.atTime <= time) + 1
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
				val data = CopyAssistant.copy(coreView.dataStoreForClass(registration.dataType).getUntyped(registration.entity))
				view.dataStoreForClass(registration.dataType).putUntyped(registration.entity, data, registration.atTime)
			})

		view.nextDataRegistrationsIndex += newDataRegistrations.size

		val nextModificationStart = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		val newEvents = coreView.events.slice(view.nextTime.time, time.asInt+1) // +1 because we want to go up to and include the given time
		view._events ++= newEvents
		val nextModificationLimit = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)

		val newModifications = coreView.modifications.slice(nextModificationStart, nextModificationLimit)
		newModifications.foreach(mod => view.applyModification(mod))
		view.modifications ++= newModifications
	}

	protected[engine] def pushEvent(event : GameEvent, state : EventState): Unit = {
		val time = nextTime
		val eventWrapper = new EventWrapper(event, time, state, coreView.modifications.size)
		coreView._events :+= eventWrapper
		currentView._events = coreView.events
	}

	def startEvent(event : GameEvent): Unit = { this.pushEvent(event, EventState.Started) }
	def continueEvent(event : GameEvent) : Unit = { this.pushEvent(event, EventState.Continues) }
	def endEvent(event : GameEvent) : Unit = { this.pushEvent(event, EventState.Ended) }
	def addEvent(event : GameEvent) : Unit = {
		this.pushEvent(event, EventState.Started)
		this.pushEvent(event, EventState.Ended)
	}

	def dataModificationLog[C <: TAuxData](entity : Entity)(implicit tag : ClassTag[C]) = {
		val rawData = coreView.data[C](entity)
		currentView.dataModificationLog[C](entity, rawData)
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

