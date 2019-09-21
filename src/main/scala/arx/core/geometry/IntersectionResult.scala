package arx.core.geometry

import arx.core.vec.ReadVec3f

trait IntersectionResult {
	def numIntersections : Int
	def intersectionPoints : List[ReadVec3f]
}

object NoIntersection extends IntersectionResult {
	override def numIntersections: Int = 0
	override def intersectionPoints: List[ReadVec3f] = Nil
}

object InfiniteIntersection extends IntersectionResult {
	override def numIntersections: Int = -1
	override def intersectionPoints: List[ReadVec3f] = Nil
}

case class Intersection(points : List[ReadVec3f]) extends IntersectionResult {
	override def numIntersections: Int = points.size
	override def intersectionPoints: List[ReadVec3f] = points
}
