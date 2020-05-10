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
	val threadpool = Executors.newCachedThreadPool(NamedThreadFactory("ArxExecutorAsync"))
	val scheduledThreadpool = Executors.newSingleThreadScheduledExecutor(NamedThreadFactory("ArxExecutorScheduled"))
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
