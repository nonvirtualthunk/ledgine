package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:28 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude

class RicherInt ( val f : Int ) extends AnyVal {
	def +- ( a : Int ) : EitherInt = { new EitherInt(f + a,f - a) }
	def -+ ( a : Int ) : EitherInt = { new EitherInt(f - a,f + a) }

	def clamp ( minimum : Int , maximum : Int ) : Int = { scala.math.min(maximum,scala.math.max(f,minimum)) }
	def isBitSet ( bitFlag : Int ) = Prelude.isBitSet(f,bitFlag)
	def anyBitsSet ( bitMask : Int ) = (f & bitMask) != 0

	def toSignedString : String = if (f >= 0) { s"+$f" } else { s"$f" }
}