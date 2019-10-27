package arx.engine.entity

import arx.core.introspection.Clazz
import arx.engine.data.TAuxData
import arx.engine.entity.Entity.{EntityAttachBuilder, EntityModifyBuilder}
import arx.engine.world.{Modifier, ModifierReference, World, WorldView}

import scala.reflect.ClassTag

class Entity(val id : Long) extends AnyVal {
	def data[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def data[T <: TAuxData](clazz : Clazz[T])(implicit view : WorldView) : T = view.data[T](clazz)(this)
	def dataOpt[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : Option[T] = view.dataOpt[T](this)
	def apply[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def apply[T <: TAuxData](clazz : Clazz[T])(implicit view : WorldView) : T = view.data[T](clazz)(this)
	def hasData[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)
	def has[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)

	def attach[T <: TAuxData](data : T)(implicit classTag : ClassTag[T]) : Entity.EntityAttachBuilder[T] = new EntityAttachBuilder[T](this, data)
	def attachI[T <: TAuxData](data : T)(initF : T => Unit)(world : World)(implicit classTag : ClassTag[T]) : Unit = {
		initF(data)
		world.attachData[T](this, data)
	}
	def modify[T <: TAuxData](modifier : Modifier[T], source : Option[String])(implicit classTag: ClassTag[T]) : Entity.EntityModifyBuilder[T] = new EntityModifyBuilder(this, modifier, source)

	override def toString: String = s"Entity($id)"

	def isSentinel = id == -1L
}



object Entity {
	val Sentinel = new Entity(-1L)

	class EntityAttachBuilder[T <: TAuxData](entity : Entity, data : T)(implicit classTag : ClassTag[T]) {
		def in(world : World): Unit = {
			world.attachData(entity, data)
		}
	}

	class EntityModifyBuilder[T <: TAuxData](entity : Entity, modifier : Modifier[T], source : Option[String])(implicit classTag : ClassTag[T]) {
		def in(world : World): ModifierReference = {
			world.modify(entity, modifier, source)
		}
	}
}