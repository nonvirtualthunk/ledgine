package arx.core.async

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/18/15
 * Time: 9:23 AM
 */

import java.util.concurrent.{Callable, Executors, ThreadFactory, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import arx.Prelude._
import arx.application.Application
import arx.core.units.UnitOfTime



object Executor {
	protected case class ExecutorThreadFactory(name : String) extends ThreadFactory {
		val s = System.getSecurityManager
		val threadGroup = s match {
			case null => Thread.currentThread().getThreadGroup
			case _ => s.getThreadGroup
		}
		val threadNumber = new AtomicInteger(0)
		val namePrefix = s"$name-thread-"

		override def newThread(r: Runnable): Thread = {
			val t = new Thread(threadGroup, r, namePrefix + threadNumber.incrementAndGet(), 0)
			if (!t.isDaemon) {t.setDaemon(true)}
			if (t.getPriority != Thread.NORM_PRIORITY) {t.setPriority(Thread.NORM_PRIORITY)}
			t
		}
	}

	val threadpool = Executors.newCachedThreadPool(ExecutorThreadFactory("ArxExecutorAsync"))
	val scheduledThreadpool = Executors.newSingleThreadScheduledExecutor(ExecutorThreadFactory("ArxExecutorScheduled"))
	private val exited = new AtomicBoolean(false)

	def submitAsync[T](func : () => T) = {
		threadpool.submit(new Callable[T] {
			override def call(): T = func()
		})
	}

	def schedulePeriodic(interval : UnitOfTime, func : () => Unit) = {
		scheduledThreadpool.schedule(new Runnable {
			override def run(): Unit = func()
		},interval.inMilliseconds.toLong, TimeUnit.MILLISECONDS)
	}

	def shutDownThreadPool () {
		threadpool.shutdownNow()
	}

	def onQuit (): Unit = {
		if (exited.compareAndSet(false, true)) {
			shutDownThreadPool()
		}
	}
}
//
//class SubmitQueue {
//	val queue = new SynchronizedQueue[() => Any]
//
//	def submit[T] (func : () => T) : Unit = {
//		val fut = Async.threadpool.submit(new Callable[T] {
//			override def call(): T = {
//				val ret = func()
//
//			}
//		})
//		fut.
//	}
//}
