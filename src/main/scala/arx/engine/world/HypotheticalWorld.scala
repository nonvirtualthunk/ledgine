package arx.engine.world
import java.util.concurrent.atomic.AtomicLong

import arx.application.Noto
import arx.core.introspection.{Clazz, CopyAssistant}
import arx.engine.data.{TAuxData, TMutableAuxData}
import arx.engine.entity.Entity
import arx.engine.event.GameEvent

import scala.reflect.ClassTag

class HypotheticalWorld(root : World, rootView : WorldView) extends World {
	onEventCallbacks = root.onEventCallbacks
	onEntityAddedCallbacks = root.onEntityAddedCallbacks
	onEntityRemovedCallbacks = root.onEntityRemovedCallbacks
	onDataAddedCallbacks = root.onDataAddedCallbacks

	entityCounter = new AtomicLong(root.entityCounter.get())
	dataRegistrations = root.dataRegistrations

	selfEntity = root.selfEntity

	root.coreView.dataStores.foreach(r => registerClass(r._1.asInstanceOf[Class[_ <: TAuxData]]))

	override protected def initializeCoreView(): WorldView = {
		new HypotheticalWorldView(root.coreView, this)
	}

	override protected def initializeCurrentView(): WorldView = {
		new HypotheticalWorldView(rootView, this)
	}

	override def eventAt(eventClock: GameEventClock): Option[EventWrapper] = {
		root.eventAt(eventClock).orElse(super.eventAt(eventClock))
	}


	override def registerClass(clazz: Class[_ <: TAuxData]): this.type = super.registerClass(clazz)

	override def createEntity(providedId: Long): Entity = {
		super.createEntity(providedId)
	}

	override def data[T <: TMutableAuxData](entity: Entity)(implicit tag: ClassTag[T]): T = ???

	override def allData(entity: Entity): Iterable[_ <: TMutableAuxData] = ???

	override def dataOpt[T <: TMutableAuxData](entity: Entity)(implicit tag: ClassTag[T]): Option[T] = ???

	override def hasData[T <: TMutableAuxData](entity: Entity)(implicit tag: ClassTag[T]): Boolean = ???

	override def worldData[T <: TMutableAuxData](implicit tag: ClassTag[T]): T = ???

	override def attachDataByClass(entity: Entity, data: TAuxData, runtimeClass: Class[_ <: TAuxData]): Unit = super.attachDataByClass(entity, data, runtimeClass)

	override def modify[T](entity: Entity, modifier: Modifier[T], source: Option[String])(implicit tag: ClassTag[T]): ModifierReference = super.modify(entity, modifier, source)

	override def view: HypotheticalWorldView = super.view.asInstanceOf[HypotheticalWorldView]

	override def viewAtTime(time: GameEventClock): WorldView = ???

	/**
	 * Update the given view to the given time. That is, after updating the final event applied in
	 * the view will have the given time.
	 */
	override def updateViewToTime(view: WorldView, time: GameEventClock): Unit = ???

	override protected[engine] def pushEvent(event: GameEvent, state: EventState): Unit = super.pushEvent(event, state)

	override def isHypothetical: Boolean = true
}

class HypotheticalWorldView(val root : WorldView, world : HypotheticalWorld) extends WorldView(world) {

	protected var modifiedEntities = Set[Entity]()
	protected var modifiedTypes = Set[Class[_ <: TAuxData]]()

	def hypotheticallyModifiedEntities = modifiedEntities
	def hypotheticallyModifiedTypes = modifiedTypes
	def isHypotheticallyModified(clazz : Clazz[_ <: TAuxData]) = modifiedTypes.contains(clazz.runtimeClass)
	def areAnyHypotheticallyModified(clazz : Clazz[_ <: TAuxData]*) = clazz.exists(c => isHypotheticallyModified(c))
	def hasHypotheticalModifications = modifiedEntities.nonEmpty


	override def entities: Iterable[Entity] = root.entities ++ super.entities

	override def nextTime: GameEventClock = super.nextTime

	override def currentTime: GameEventClock = super.currentTime

	def hypotheticalEvents = super.wrappedEvents

	override def events: Iterable[GameEvent] = root.events ++ super.events

	override def wrappedEvents: Vector[EventWrapper] = root.wrappedEvents ++ super.wrappedEvents

	override def entitiesWithData[T <: TAuxData](implicit tag: ClassTag[T]): Iterable[Entity] = {
		super.entitiesWithData ++ dataStore[T].entities
	}

	override def dataByClass[T <: TAuxData](entity: Entity, clazz: Class[T]): T = {
		super.dataOptByClass(entity, clazz).getOrElse(root.dataByClass(entity, clazz))
	}

	override def dataOptByClass[T](entity: Entity, clazz: Class[T]): Option[T] = {
		super.dataOptByClass(entity, clazz).orElse(root.dataOptByClass(entity, clazz))
	}

	override def hasDataByClass[T <: TAuxData](entity: Entity, runtimeClass: Class[_]): Boolean = {
		super.hasDataByClass(entity, runtimeClass) || root.hasDataByClass[T](entity, runtimeClass)
	}

	override def applyOverlayModification(modification: Modification): Unit = ???

	override def clearOverlay(): Unit = ???

	override protected[engine] def applyModification(modification: Modification): Unit = {
		val dataStore = dataStores(modification.modifiedType)
		// if this world does not yet have a copy of the entity data we're modifying, make one. Copy on write, effectively
		if (!dataStore.contains(modification.entity)) {
			modifiedEntities += modification.entity
			modifiedTypes += modification.modifiedType.asInstanceOf[Class[_ <: TAuxData]]
			val (baseData, worldTime) = root.dataStores(modification.modifiedType).getRaw(modification.entity) match {
				case Some(rootData) => (rootData.data, rootData.addedAt)
				case None =>
					Noto.error(s"modification to hypothetical world without registration: $modification")
					(dataStore.sentinel, currentTime)
			}
			val newData = CopyAssistant.copy(baseData)
			dataStore.putUntyped(modification.entity, newData, worldTime)
		}
		super.applyModification(modification)
	}

	override protected[engine] def copyAtTime(atTime: GameEventClock): WorldView = ???

	override def dataModificationLog[C <: TAuxData](entity: Entity, baseValue: C)(implicit tag: ClassTag[C]): DataModificationLog[C] = ???

	override def dataModificationLog[C <: TAuxData](entity: Entity)(implicit tag: ClassTag[C]): DataModificationLog[C] = ???

}