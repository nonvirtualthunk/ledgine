package arx.core.richer

class RicherTraversable[T](val intern : Traversable[T]) extends AnyVal {

	def sumOf[N : Numeric](f : T => N) : N = {
		intern.map(f).sum
	}

}


class RicherIterable[T](val intern : Iterable[T]) extends AnyVal {
	def findFirstWith[U] ( f : (T) => Option[U] ) : Option[(T,U)] = {
		val i = intern.iterator
		while ( i.hasNext ) {
			val e = i.next()
			f(e) match {
				case Some(v) => return Some((e,v))
				case _ =>
			}
		}
		None
	}
}