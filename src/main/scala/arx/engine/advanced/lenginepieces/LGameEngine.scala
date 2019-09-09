package arx.engine.advanced.lenginepieces

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/17/18
  * Time: 8:00 AM
  */

import arx.engine.EnginePiece
import arx.engine.advanced.lenginecomponents.LGameComponent
import arx.engine.event.EventBus
import arx.engine.world.World

class LGameEngine (val world: World, val eventBus : EventBus) extends EnginePiece[World, LGameComponent] {
	val self = this

	parallelism = 4

	override def initialize(serial: Boolean): Unit = {
		super.initialize(serial)
	}

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef = {
		l.find(c => try {c.getConstructor(classOf[LGameEngine]) != null} catch {case e: Exception => false}) match {
			case Some(clazz) =>
				val constructor = clazz.getConstructor(classOf[LGameEngine])
				constructor.newInstance(this).asInstanceOf[LGameComponent]
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}
}