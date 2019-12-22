package arx.engine.entity

import arx.engine.world.World

object EntityCopier {
	def copyEntity(world : World, entity: Entity) : Entity = {
		world.copyEntity(entity)
	}
}
