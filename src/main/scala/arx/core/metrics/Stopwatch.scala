package arx.core.metrics

import arx.Prelude._

class Stopwatch {
	val startTime = System.nanoTime()

	def duration = (System.nanoTime() - startTime).nanoseconds
}

object Stopwatch {
	def start() = new Stopwatch
}
