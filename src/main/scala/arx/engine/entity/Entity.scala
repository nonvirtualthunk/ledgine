package arx.engine.entity

import arx.core.introspection.Clazz
import arx.engine.data.TAuxData
import arx.engine.world.WorldView

import scala.reflect.ClassTag

class Entity(val id : Long) extends AnyVal {
	def data[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def data[T <: TAuxData](clazz : Clazz[T])(implicit view : WorldView) : T = view.data[T](clazz)(this)
	def dataOpt[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : Option[T] = view.dataOpt[T](this)
	def apply[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def apply[T <: TAuxData](clazz : Clazz[T])(implicit view : WorldView) : T = view.data[T](clazz)(this)
	def hasData[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)
	def has[T <: TAuxData](implicit view : WorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)

	override def toString: String = s"LEntity($id)"

	def isSentinel = id == -1L
}



object Entity {
	val Sentinel = new Entity(-1L)
}