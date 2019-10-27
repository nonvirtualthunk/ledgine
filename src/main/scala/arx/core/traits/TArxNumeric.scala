package arx.core.traits

import scala.annotation.tailrec

trait TArxNumeric[T] {
	def +(other : T) : T
	def -(other : T) : T
	def *(other : Float) : T
	def zero : T
}

object ArxNumeric {
	@tailrec
	def toDouble(a : Any) : Option[Double] = a match {
			case Some(w) => toDouble(w)
			case b : Byte => Some(b.toDouble)
			case s : Short => Some(s.toDouble)
			case i : Int => Some(i.toDouble)
			case l : Long => Some(l.toLong)
			case f : Float => Some(f.toDouble)
			case d : Double => Some(d)
			case _ => None
	}
}