package arx.core.synchronization

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/22/14
 * Time: 12:11 PM
 */

import java.util.concurrent.locks.ReentrantReadWriteLock

import arx.Prelude._


class ReadWriteLock {
	protected val intern = new ReentrantReadWriteLock
	def readLock[T] (func : => T): T = {
		intern.readLock().lock()

		try {
			val ret = func
			ret
		} finally {
			intern.readLock().unlock()
		}
	}

	def writeLock[T] (func : => T): T = {
		intern.writeLock().lock()

		try {
			func
		} finally {
			intern.writeLock().unlock()
		}
	}
}
