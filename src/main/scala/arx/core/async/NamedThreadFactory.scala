package arx.core.async

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

case class NamedThreadFactory(name : String) extends ThreadFactory {
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
