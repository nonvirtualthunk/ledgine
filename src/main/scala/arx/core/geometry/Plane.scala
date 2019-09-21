package arx.core.geometry

/**
  * Created by IntelliJ IDEA.
  * User: nvt
  * Date: 7/13/12
  * Time: 3:29 PM
  * Created by nonvirtualthunk
  */

import arx.Prelude._
import arx.core.mat.{Mat3x3, MatrixFunctions}
import arx.core.vec.{ReadVec3f, Vec3f}

class Plane ( var point : ReadVec3f , var normal : ReadVec3f ) {
	def intersect(start: ReadVec3f, end: ReadVec3f) = {
		val delta = end - start
		val under = delta.dot(normal)
		if ( absf(under) < 0.00001f ) {
			NoIntersection
		} else {
			val t = (point - start).dot(normal) / under
			Intersection(List(start + delta * t))
		}
	}
}

class Quad ( center : ReadVec3f , xVector : ReadVec3f , yVector : ReadVec3f ) extends Plane (center,xVector.cross(yVector).normalizeSafe) {
	override def intersect(start: ReadVec3f, end: ReadVec3f) = {
		val rawIntersect = super.intersect(start, end)
		val numIntersects = rawIntersect.numIntersections
		if ( numIntersects == 0 ) { rawIntersect }
		else {
			val intersectionPoint = rawIntersect.intersectionPoints.head - center
			val txVector = Vec3f(xVector.x,xVector.y,xVector.z)
			val tyVector = Vec3f(yVector.x,yVector.y,yVector.z)
			val tnormal = Vec3f(normal.x,normal.y,normal.z)
			val basis = Mat3x3(txVector,tyVector,tnormal)
			val invBasis = MatrixFunctions.inverse(basis)
			val tintersectionPoint = Vec3f(intersectionPoint.x,intersectionPoint.y,intersectionPoint.z)

			val relativeCoord = invBasis * tintersectionPoint
			if ( absf(relativeCoord.x) <= 0.5f && absf(relativeCoord.y) <= 0.5f ) {
				rawIntersect
			} else {
				NoIntersection
			}
		}
	}
}

class UprightBillboard( center : ReadVec3f , orthoDim : Float , upDim : Float ) {
	def intersect(start: ReadVec3f, end: ReadVec3f) = {
		val up = Vec3f(0.0f,0.0f,1.0f)
		val delta = (end - start).normalize
		val ortho = delta.cross(up)
		val quad = new Quad(center,ortho * orthoDim,up * upDim)
		quad.intersect(start,end)
	}
}