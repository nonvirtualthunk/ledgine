package arx.core.math

import arx.Prelude
import arx.application.Noto
import arx.core.traits.TArxNumeric
import arx.core.units.{RatioUnitOfMeasure, UnitOfMeasure, UnitOfTime}
import arx.core.vec.coordinates.{CartVec, CartVec3}
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec3f, ReadVec4f, ReadVec4i, Vec2i, Vec4f, Vec4i}
import arx.engine.data.Reduceable
import arx.graphics.helpers.HSBA

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait Interpolation[T] {
	def interpolate(pcnt : Float) : T

	def interpolate(startTime : UnitOfTime, duration : UnitOfTime, curTime : UnitOfTime) : T =
		interpolate((curTime - startTime).inSeconds / duration.inSeconds)

	def sin010 : Interpolation[T] = {
		val outer = this
		new Interpolation[T] {
			override def interpolate(pcnt : Float) : T = {
				outer.interpolate(Prelude.sinf(pcnt * math.Pi))
			}
		}
	}

	def sin01 : Interpolation[T] = {
		val outer = this
		new Interpolation[T] {
			override def interpolate(pcnt : Float) : T = {
				outer.interpolate(Prelude.sinf(pcnt * math.Pi * 0.5f))
			}
		}
	}

	def sin10 : Interpolation[T] = {
		val outer = this
		new Interpolation[T] {
			override def interpolate(pcnt : Float) : T = {
				outer.interpolate(Prelude.sinf(math.Pi * 0.5f + pcnt * math.Pi * 0.5f))
			}
		}
	}

	def map[U](f : T => U) : Interpolation[U] = {
		val outer = this
		new Interpolation[U] {
			override def interpolate(pcnt: Float): U = {
				f(outer.interpolate(pcnt))
			}
		}
	}
}

class InterpolationBuilder[T] {
	import arx.Prelude._

	private var _keyframes : Seq[T] = Nil
	private var _curve : Float => Float = identity
	private var _applicationFunction : (T,T,Float) => T = (a,_,_) => a
	private var _rawFunction : Float => T = _

	def toInterpolation: Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = {
			val t = _curve(pcnt)

			if (_keyframes.nonEmpty) {
				val startIndex = (t * _keyframes.size).toInt.clamp(0, _keyframes.size - 1)
				val endIndex = (startIndex + 1).clamp(0, _keyframes.size - 1)

				val startF = startIndex / (_keyframes.size - 1).toFloat
				val endF = endIndex / (_keyframes.size - 1).toFloat

				val localT = (t - startF) / (endF - startF)
				_applicationFunction(_keyframes(startIndex), _keyframes(endIndex), localT)
			} else if (_rawFunction != null) {
				_rawFunction(t)
			} else {
				???
			}
		}
	}

	def curve(func : Float => Float) : this.type = {
		_curve = func
		this
	}
	def keyframes(frames : T*) : this.type = {
		_keyframes = frames.toSeq
		this
	}
	def apply(func : (T,T, Float) => T) : this.type = {
		_applicationFunction = func
		this
	}
	def rawFunction(func : Float => T) : this.type = {
		_rawFunction = func
		this
	}
}

//class LinearInterpolation[T <: { def -(a : T) : T; def * (f : Float) : T; def +(a : T) : T }](a : T, b : T) extends Interpolation[T] {
//	override def interpolate(pcnt: Float): T = a + (b - a) * pcnt
//}

object Interpolation {
	implicit def fromBuilder[T] (builder : InterpolationBuilder[T]) : Interpolation[T] = builder.toInterpolation

	def fromFunction[T](f : Float => T) : InterpolationBuilder[T] = {
		new InterpolationBuilder[T]().rawFunction(f)
	}

	def apply[T](keyframes : T*)(implicit classTag: ClassTag[T]) : InterpolationBuilder[T] = {
		val builder = new InterpolationBuilder[T]
		builder.keyframes(keyframes : _*)
		if (classTag.runtimeClass == classOf[Float]) {
			builder.apply(((a:Float,b:Float, t :Float) => a + (b - a) * t).asInstanceOf[(T,T,Float) => T])
		} else if (classTag.runtimeClass == classOf[ReadVec3f]) {
			builder.apply(((a:ReadVec3f,b:ReadVec3f, t :Float) => a + (b - a) * t).asInstanceOf[(T,T,Float) => T])
		}
		builder
	}

	def constant[T](v : T) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = v
	}

	def between(a : Int, b : Int) : Interpolation[Int] = new Interpolation[Int] {
		override def interpolate(pcnt: Float): Int = (a.toFloat + (b-a) * pcnt).round
	}

	def between(a : Float, b : Float) : Interpolation[Float] = new Interpolation[Float] {
		override def interpolate(pcnt: Float): Float = a + (b-a) * pcnt
	}

	def between(a : ReadVec2i, b : ReadVec2i) : Interpolation[ReadVec2i] = new Interpolation[ReadVec2i] {
		override def interpolate(pcnt: Float): ReadVec2i = (a + (b-a) * pcnt).round
	}

	def between(a : ReadVec2f, b : ReadVec2f) : Interpolation[ReadVec2f] = new Interpolation[ReadVec2f] {
		override def interpolate(pcnt: Float): ReadVec2f = a + (b-a) * pcnt
	}

	def between(a : ReadVec2f, b : ReadVec2f, c : ReadVec2f) : Interpolation[ReadVec2f] = new Interpolation[ReadVec2f] {
		override def interpolate(pcnt: Float): ReadVec2f = if (pcnt < 0.5f) {
			a + (b-a) * pcnt * 2.0f
		} else {
			b + (c-b) * (pcnt - 0.5f) * 2.0f
		}
	}

	def between(a : CartVec3, b : CartVec3) : Interpolation[CartVec3] = new Interpolation[CartVec3] {
		override def interpolate(pcnt: Float): CartVec3 = a + (b-a) * pcnt
	}

	def between(a : HSBA, b : HSBA) : Interpolation[HSBA] = new Interpolation[HSBA] {
		override def interpolate(pcnt: Float): HSBA = HSBA(a + (b-a) * pcnt)
	}

	def between[T <: TArxNumeric[T]](a : T , b : T) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = a + (b-a) * pcnt
	}

	def betweenI(a : Reduceable[Int], b : Reduceable[Int]) : Interpolation[Reduceable[Int]] = new Interpolation[Reduceable[Int]] {
		override def interpolate(pcnt: Float): Reduceable[Int] = new Reduceable[Int]((a.baseValue + (b.baseValue - a.baseValue) * pcnt).round, (a.reducedBy + (b.reducedBy - a.reducedBy) * pcnt).round)
	}

//	def between[T : Numeric](a : Reduceable[T], b : Reduceable[T]) : Interpolation[Reduceable[T]] = new Interpolation[Reduceable[T]] {
//		override def interpolate(pcnt: Float): Reduceable[T] = {
//			val NUM = implicitly[Numeric[T]]
//			new Reduceable[T]((NUM.plus(a.baseValue, NUM.toFloat(NUM.minus(b.baseValue, a.baseValue)) * pcnt)))
//		}
//	}


	def frames[T](frames : Seq[T]) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = {
			val index = (frames.size * pcnt).toInt.min(frames.size-1)
			frames(index)
		}
	}

	implicit def toConstantInterpolation[T](v : T) : Interpolation[T] = constant(v)

	implicit def functionInterpolation[T](f : Float => T) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = f(pcnt)
	}


	protected def interpolateCosF ( x : Float , controlPoints : Seq[(Float,_)] ) : (Int,Int,Float) = {
		controlPoints match {
			case Nil => Noto.warn("can't interpolate with no control points");(0,0,0.0f)
			case one :: Nil => (0,0,0.0f)
			case someList => {
				val cp = someList.sortBy( _._1 )
				val b = cp.indexWhere( _._1 >= x ) match { case -1 => cp.size-1 ; case i => i }
				val a = cp.lastIndexWhere( _._1 < x ) match { case -1 => 0 ; case i => i }
				val mu = (x - controlPoints(a)._1) / scala.math.max(0.0001f,controlPoints(b)._1 - controlPoints(a)._1)
				val mu2 = (1.0f - scala.math.cos(mu * scala.math.Pi).toFloat) * 0.5f
				(a,b,mu2)
			}
		}
	}
	protected def interpolateLinF ( x : Float , controlPoints : Seq[(Float,_)] ) : (Int,Int,Float) = {
		controlPoints match {
			case Nil => Noto.warn("can't interpolate with no control points");(0,0,0.0f)
			case one :: Nil => (0,0,0.0f)
			case someList => {
				val cp = someList.sortBy( _._1 )
				val b = cp.indexWhere( _._1 >= x ) match { case -1 => cp.size-1 ; case i => i }
				val a = cp.lastIndexWhere( _._1 < x ) match { case -1 => 0 ; case i => i }
				val mu = (x - controlPoints(a)._1) / scala.math.max(0.0001f,controlPoints(b)._1 - controlPoints(a)._1)
				(a,b,mu)
			}
		}
	}
	protected def interpolateStepF ( x : Float , controlPoints : Seq[(Float,_)] ) : Int = {
		controlPoints.lastIndexWhere( _._1 <= x ) match {
			case -1 => controlPoints.size - 1
			case i => i
		}//0.0f 0.5f 1.0f
	}

	def linInterpolate ( x : Float , controlPoints : Seq[(Float,Float)] ) : Float = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def linInterpolatei ( x : Float , controlPoints : Seq[(Float,Int)] ) : Float = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def linInterpolate ( x : UnitOfMeasure[_] , controlPoints : Seq[(UnitOfMeasure[_],Float)] ) : Float = {
		linInterpolate(x.toBaseUnitOfMeasure,controlPoints.map(tup => tup._1.toBaseUnitOfMeasure -> tup._2))
	}
	def linInterpolate ( x : RatioUnitOfMeasure[_,_] , controlPoints : Seq[(RatioUnitOfMeasure[_,_],Float)] ) : Float = {
		linInterpolate(x.toBaseUnitOfMeasure,controlPoints.map(tup => tup._1.toBaseUnitOfMeasure -> tup._2))
	}
	def linInterpolatev4 ( x : Float , controlPoints : Seq[(Float,Vec4f)] ) = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def linInterpolatev4i ( x : Float , controlPoints : Seq[(Float,Vec4i)] ) = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		val v = controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
		Vec4i(v.r.toInt,v.g.toInt,v.b.toInt,v.a.toInt)
	}
	def linInterpolatev3 ( x : Float , controlPoints : Seq[(Float,ReadVec3f)] ) = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}

	def cosInterpolate ( x : Float , controlPoints : Seq[(Float,Float)] ) = {
		val (ai,bi,mu2) = interpolateCosF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def cosInterpolatev4 ( x : Float , controlPoints : Seq[(Float,ReadVec4f)] ) : ReadVec4f = {
		val (ai,bi,mu2) = interpolateCosF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def cosInterpolatev3 ( x : Float , controlPoints : Seq[(Float,ReadVec3f)] ) : ReadVec3f = {
		val (ai,bi,mu2) = interpolateCosF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}

	def stepInterpolate4i ( x : Float , controlPoints : Seq[(Float,ReadVec4i)] ) = {
		require(controlPoints.nonEmpty)
		val index = interpolateStepF(x,controlPoints)
		controlPoints(index)._2
	}
	def stepInterpolate[T] ( x : Float , controlPoints : Seq[(Float,T)] ) = {
		require(controlPoints.nonEmpty)
		val index = interpolateStepF(x,controlPoints)
		controlPoints(index)._2
	}

	def sin010(pcnt : Float) = Prelude.sinf(pcnt * math.Pi)
}