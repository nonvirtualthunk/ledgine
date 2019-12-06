package arx.engine.control.components.windowing.widgets

import arx.engine.entity.Taxon
import arx.graphics.TToImage
import arx.resource.ResourceManager

import scala.collection.mutable

trait SpriteProvider {


	def getSpriteDefinitionFor(t : Taxon) : Option[SpriteDefinition]
	def spriteDefinitionFor(t : Taxon) : SpriteDefinition
}



case class SpriteDefinition(icon : TToImage, icon16 : TToImage) {}