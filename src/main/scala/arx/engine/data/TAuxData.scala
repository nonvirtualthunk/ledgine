package arx.engine.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:19 AM
 */

import java.util.concurrent.atomic.AtomicInteger

import arx.Prelude._
import arx.core.introspection.{CopyAssistant, ReflectionAssistant}



trait TAuxData extends ConfigLoadable {

	def initBy(f : (this.type) => Unit): this.type = {
		f(this)
		this
	}

	override def toString() = {
		this.getClass.getDeclaredFields.map(f => {
			s"${f.getName} : ${ReflectionAssistant.getFieldValue(this, f)}"
		}).mkString("\n")
	}
}

trait TMutableAuxData extends TAuxData

trait TTestAuxData extends TAuxData


/**
 * Not top level aux data, but intended to be nested within it, can generate companions
 */
trait TNestedData {

}