package arx.core.async

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/30/12
 * Time: 12:54 PM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.LockSupport

abstract class KillableThread(var level:KillableThread.Level) extends Thread {
	KillableThread.threads ::= this

	var ended = new AtomicBoolean(false)
	var finishComplete = new AtomicBoolean(false)

	final override def run() {
		while ( ! ended.get() ) {
			whileRunningDo()
		}
		finishComplete.set(true);
	}

	def whileRunningDo ()

	def end () {
		kill()
	}
	def kill () {
		if ( ended.compareAndSet(false, true) ) {
			if (!Thread.currentThread().equals(this)) {
				interrupt()
				while (!finishComplete.get()) {
					LockSupport.parkNanos(1000000)
				}
			}
		}
	}

	def andStart() : this.type = {
		start()
		this
	}
}
object KillableThread {
	trait Level { val ordinal : Int }
	case object ApplicationLevel extends Level { val ordinal = 10 }
	case object GameLevel extends Level { val ordinal = 5 }
	case object TemporaryLevel extends Level { val ordinal = 1 }

	var threads = List[KillableThread]()

	/**
	 * Kills all threads at or below the given level
	 */
	def kill (level:Level) {
		threads.foreach ( t => if ( t.level.ordinal <= level.ordinal ) { t.kill() } )
	}
}
