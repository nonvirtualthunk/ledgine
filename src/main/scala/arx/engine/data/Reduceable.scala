package arx.engine.data

case class Reduceable[T : Numeric](baseValue : T,reducedBy : T) {
	def this(baseValueIn : T) {
		this(baseValueIn, implicitly[Numeric[T]].zero)
	}
	private def NUM = implicitly[Numeric[T]]

	def currentValue = NUM.minus(baseValue, reducedBy)
	def maxValue = baseValue
	def reduceBy(n : T, limitToZero : Boolean) = {
		var newReducedBy = NUM.plus(reducedBy, n)
		if (limitToZero) {
			newReducedBy = NUM.min(newReducedBy, baseValue)
		}
		new Reduceable(baseValue, newReducedBy)
	}
	def reduceTo(n : T) = {
		new Reduceable(baseValue, NUM.minus(baseValue, n))
	}
	def recoverBy(n : T, limitToZero : Boolean) = {
		var newReducedBy = NUM.minus(reducedBy, n)
		if (limitToZero) {
			newReducedBy = NUM.max(newReducedBy, NUM.zero)
		}
		new Reduceable(baseValue, newReducedBy)
	}
	def recoverToFull() = new Reduceable(baseValue)

	override def toString: String = {
		s"$currentValue ($maxValue)"
	}

	def withBaseValue(v : T) : Reduceable[T] = {
		new Reduceable(v, reducedBy)
	}
}

object Reduceable {
	def apply[T : Numeric](baseValue : T) = new Reduceable(baseValue)
}