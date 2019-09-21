package arx.engine.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:19 AM
 */

import java.util.concurrent.atomic.AtomicInteger

import arx.Prelude._
import arx.core.introspection.CopyAssistant



trait TAuxData {

	def withData(f : (this.type) => Unit): Unit = {
		f(this)
	}
}

trait TMutableAuxData extends TAuxData