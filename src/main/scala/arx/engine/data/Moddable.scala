package arx.engine.data

trait Moddable[+T] {
	def resolve(): T

	def dynamic: Boolean = true

	def baseValue(): T

	override def equals(p1: Any) = p1 match {
		case null => false
		case m: Moddable[_] => m.resolve() == this.resolve()
		case o => this.resolve() == o
	}
}

class FunctionModdable[T](val func : () => T) extends Moddable[T] {
	override def dynamic: Boolean = true

	override def resolve(): T = func()

	override def baseValue() = func()

	override def toString: String = { resolve().toString }

	override def equals(p1: Any) = p1 match {
		case null => false
		case m: Moddable[T] => m.resolve() == this.resolve()
		case o => this.resolve() == o
	}
}

class ValueModdable[T](val value: T) extends Moddable[T] {
	override def dynamic: Boolean = false

	override def resolve(): T = value

	override def baseValue() = value

	override def toString: String = { resolve().toString }

	override def equals(p1: Any) = p1 match {
		case null => false
		case m: Moddable[T] => m.resolve() == this.resolve()
		case o => this.resolve() == o
	}
}


object Moddable{
	protected val moddable0 = new ValueModdable(0.0f)
	protected val moddable1 = new ValueModdable(1.0f)
	protected val moddable0i = new ValueModdable[Int](0)
	protected val moddable1i = new ValueModdable[Int](1)
	protected val moddableNil = new ValueModdable(Nil)

	def apply ( value : Float ) : Moddable[Float] = {
		if ( value == 0.0f ) { moddable0 }
		else if ( value == 1.0f ) { moddable1 }
		else { new ValueModdable(value) }
	}
	def apply ( value : Int ) : Moddable[Int] = {
		if ( value == 0 ) { moddable0i }
		else if ( value == 1 ) { moddable1i }
		else { new ValueModdable[Int](value) }
	}
	def apply[T] ( nil : Nil.type ): Moddable[Nil.type] = { moddableNil }
	def apply[T] ( value: T ): Moddable[T] = { new ValueModdable[T](value) }
	def apply[T] ( func: () => T ): Moddable[T] = { new FunctionModdable[T](func) }

}