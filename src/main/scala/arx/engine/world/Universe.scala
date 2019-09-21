package arx.engine.world

/**
 * Collection of worlds, shared by all engines
 */
class Universe {
	var worlds = Map[AnyRef, World]()

	def getOrCreateWorld(key : AnyRef) : World = synchronized {
		worlds.get(key) match {
			case Some(world) => world
			case None =>
				val w = new World
				worlds += key -> w
				w
		}
	}
}
