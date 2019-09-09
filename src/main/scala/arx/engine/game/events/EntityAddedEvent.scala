package arx.engine.game.events

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.control.event.Event
import arx.engine.entity.TGameEntity
import arx.engine.world.Entity


case class EntityAddedEvent(entity : TGameEntity) extends Event {
}

case class EntityRemovedEvent(entity : TGameEntity) extends Event {

}