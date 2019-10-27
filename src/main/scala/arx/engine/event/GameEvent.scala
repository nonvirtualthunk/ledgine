package arx.engine.event

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 2:36 PM
  */

import arx.Prelude._
import arx.core.vec._
import arx.engine.world.{EventState, World}

class GameEvent extends Event {
	val createdAtWallTime = curTime()
	var state : EventState = EventState.Started
	var world : World = _

	def withWorld(w: World) : this.type = {
		this.world = w
		this
	}
}

class WorldCreatedEvent extends GameEvent {}