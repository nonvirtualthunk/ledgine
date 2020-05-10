package arx.core.metrics

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import arx.Prelude
import arx.core.units.UnitOfTime
import com.codahale.metrics.{ExponentiallyDecayingReservoir, Histogram, Timer}
import overlock.atomicmap.AtomicMap

class Transaction(name : String) {
  def start() = new TransactionTracker(this)

  val count = new AtomicInteger(0)
  val duration = new Timer()
  val segments = AtomicMap.atomicNBHM[String, Histogram]

  protected[metrics] def recordResults(tracker : TransactionTracker): Unit = {
    val totalDuration = (tracker.endTime - tracker.startTime)
    duration.update(totalDuration.inNanoseconds.toLong, TimeUnit.NANOSECONDS)
    for ((segName, duration) <- tracker.segments) {
      val pcnt = if (totalDuration.inSeconds > 0.0f) {
        duration.inSeconds / totalDuration.inSeconds
      } else {
        0.0f
      }

      val tim = segments.getOrElseUpdate(segName, new Histogram(new ExponentiallyDecayingReservoir()))
      tim.update((pcnt * 100000).toInt)
    }
    count.incrementAndGet()
  }

  def prettyPrint(): Unit = {
    val snap = duration.getSnapshot

    println(s"$name")
    println(s"             count = ${duration.getCount}")
    println(s"            median = ${snap.getMedian / 1e6} millis")
    println(s"              mean = ${snap.getMean / 1e6} millis")
    println(s"               99% = ${snap.get99thPercentile / 1e6} millis")
    println(s"             99.9% = ${snap.get999thPercentile / 1e6} millis")
    for ((segment, timer) <- segments) {
      val subSnap = timer.getSnapshot
      println(s"      $segment")
      if (duration.getCount != 0) {
        println(s"               invoked = ${(timer.getCount.toFloat / duration.getCount.toFloat) * 100}%")
      }
      if (snap.getMedian != 0.0f) {
        println(s"                median = ${(subSnap.getMedian / 1000)}%")
      }
      if (snap.getMean != 0.0f) {
        println(s"                  mean = ${(subSnap.getMean / 1000)}%")
      }
      if (snap.get99thPercentile() != 0.0f) {
        println(s"                   99% = ${(subSnap.get99thPercentile / 1000)}%")
      }
      if (snap.get999thPercentile() != 0.0f) {
        println(s"                 99.9% = ${(subSnap.get999thPercentile / 1000)}%")
      }
    }

  }
}

class TransactionTracker(txn : Transaction) {
  val startTime = Prelude.curTime()
  var endTime = startTime

  var segments = Map[String,UnitOfTime]()

  def timeSegment(name : String)(stmt : => Unit): Unit = {
    val start = Prelude.curTime()

    stmt

    val end = Prelude.curTime()
    segments += name -> (end - start)
  }

  def end(): Unit = {
    endTime = Prelude.curTime()

    txn.recordResults(this)
  }
}
