package arx.engine.game.events

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.entity.Entity
import arx.engine.event.Event


case class EntityAddedEvent(entity : Entity) extends Event {
}

case class EntityRemovedEvent(entity : Entity) extends Event {

}