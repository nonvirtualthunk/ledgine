package arx.core.metrics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/14
 * Time: 4:34 PM
 */

import java.util
import java.util.concurrent.{Callable, TimeUnit}

import arx.Prelude._
import arx.core.units.UnitOfTime
import com.codahale.metrics.{ConsoleReporter, Counter, Gauge, Histogram, Meter, MetricRegistry, Timer}


object Metrics extends MetricRegistry {
	val registry = this

	private var checkpoints = List[(String, UnitOfTime)]()

	def stateGauge[T](name : String,initialState : T) = {
		val tmp = new StateGauge[T](initialState)
		registry.register(name,tmp)
		tmp
	}

	class RichTimer ( timer : Timer ) {
		def timeStmt[T] (stmt : => T): T = {
			timer.time(new Callable[T] {
				override def call(): T = {
					stmt
				}
			})
		}
	}

	class StateGauge[T](var state : T) extends Gauge[T] {
		override def getValue: T = state

		def set (s : T) { state = s }
	}
	
	
	def prettyPrint (): Unit = {
		val reporter = ConsoleReporter.forRegistry(registry)
			.convertRatesTo(TimeUnit.SECONDS)
			.convertDurationsTo(TimeUnit.MILLISECONDS)
			.build()
		reporter.report(registry.getGauges,registry.getCounters,registry.getHistograms,registry.getMeters,registry.getTimers)
		println("Checkpoints:")
		checkpoints.reverse.foreach {
			case (name, time) => println(s"\t$name : ${time.inSeconds}s")
		}
	}
	def prettyPrintTimer (name : String): Unit = {
		val timer = registry.getTimers.get(name)
		prettyPrintTimer(timer, name)
	}
	def prettyPrintTimer(timer : Timer, name : String = "Timer") : Unit = {
		val reporter = ConsoleReporter.forRegistry(registry)
			.convertRatesTo(TimeUnit.SECONDS)
			.convertDurationsTo(TimeUnit.MILLISECONDS)
			.build()

		val tmap = new util.TreeMap[String,Timer]
		tmap.put(name,timer)

		reporter.report(new util.TreeMap[String,Gauge[_]],new util.TreeMap[String,Counter],new util.TreeMap[String,Histogram],new util.TreeMap[String,Meter],tmap)
	}

	def checkpoint(name : String): Unit = {
		checkpoints ::= (name, curTime())
	}
}
