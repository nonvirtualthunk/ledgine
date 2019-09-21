package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:31 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfMeasure
import scala.collection.parallel.{ParSetLike, ParIterableLike, TaskSupport}

class ArxSet[T](val intern : Set[T])  {
	def ofType [E <: AnyRef : Manifest] : Set[E] = {
		intern.collect { case e if ( manifest[E].erasure.isAssignableFrom(e.getClass) ) => e.asInstanceOf[E] }
	}
	def usum[U <: UnitOfMeasure[U]]( f : (T) => U )(implicit start : U) : U = { intern.map( v => f(v) ).foldLeft(start)( (a,b) => a + b ) }
}