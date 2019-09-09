package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/12
 * Time: 6:19 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import collection.{mutable, GenTraversableOnce}
import arx.core.vec.coordinates.VoxelCoord

class ProbabilisticHashSet[T](expectedSize : Int,falsePositiveRate : Float) extends TBareBonesSet[T] {
	val bloom = BloomFilter.withFalsePositiveProbability(expectedSize,falsePositiveRate)
	val list = new mutable.ArrayBuffer[T](expectedSize)

	if ( bloom.expectedFalsePositiveProbability > falsePositiveRate ) { Noto.info("Bloom filter had unexpectedly high FP rate : " + bloom.expectedFalsePositiveProbability ) }

	var incrementor = ProbabilisticHashSet.incrementor
	ProbabilisticHashSet.incrementor = (ProbabilisticHashSet.incrementor + 1) & 127

	def hash ( t : T ) = t.hashCode() + incrementor

	def contains ( t : T ) = bloom.contains(hash(t))

	def add ( t : T ) : Boolean = {
		val h = hash(t)
		if ( ! bloom.contains(h) ) {
			bloom.add(h)
			list.append(t)

			if ( list.size > expectedSize ) {
				Noto.info("Size has exceeded expectation : " + list.size + " > " + expectedSize)
			}
			true
		} else { false }
	}

	def addAll ( set : TBareBonesSet[T] ) {
		for ( elem <- set ) {
			add(elem)
		}
	}

	override def isEmpty = list.isEmpty

	override def values = list

	def foreach[U](f: (T) => U) {
		values.foreach(f)
	}

	override def size = list.size

	def addCertain(t: T) { list.append(t) }
}
object ProbabilisticHashSet {
	var incrementor = 0
}

trait TBareBonesSet[T] extends Traversable[T] {
	def contains ( t : T ) : Boolean
	def add ( t : T ) : Boolean
	def addCertain ( t : T )
	def addAll ( t : TBareBonesSet[T] )
	def values : Traversable[T] = this
}