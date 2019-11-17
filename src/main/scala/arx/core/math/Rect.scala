
package arx.core.math

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/15
 * Time: 8:48 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.{ReadVec2f, ReadVec2i, Vec2f, Vec2i}
import arx.graphics.Axis
import javax.print.attribute.standard.MediaSize.Other



case class Rectf (var x : Float, var y : Float, var width : Float, var height : Float) {
	def maxX = x + w
	def minX = x
	def maxY = y + h
	def minY = y
	def centerX = x + w * 0.5f
	def centerY = y + h * 0.5f

	def w = width
	def h = height

	def min = ReadVec2f(minX,minY)
	def max = ReadVec2f(maxX,maxY)

	def intersect (r2 : Rectf) : Rectf = {
		val r1 = this
		val lx = math.max(r1.x,r2.x)
		val ly = math.max(r1.y,r2.y)
		val hx = math.min(r1.x + r1.w,r2.x + r2.w)
		val hy = math.min(r1.y + r1.h,r2.y + r2.h)
		Rectf(lx,ly,hx-lx,hy-ly)
	}

	def translate(offset : ReadVec2f) : Rectf = {
		Rectf(x + offset.x, y + offset.y, w, h)
	}

	def toRecti : Recti = Recti(x.toInt,y.toInt,w.toInt,h.toInt)
	def xy = Vec2f(x,y)
	def dimensions = Vec2f(width,height)

}

object Rectf {
	def apply(r : Rectf) : Rectf = {
		Rectf(r.x,r.y,r.width,r.height)
	}
	def apply(r : Recti) : Rectf = {
		Rectf(r.x,r.y,r.width,r.height)
	}

	def fromMinAndMax(min : ReadVec2f, max : ReadVec2f) = {
		Rectf(min.x,min.y,max.x - min.x, max.y - min.y)
	}
}

case class Recti (var x : Int, var y : Int, var width : Int, var height : Int) {
	def intersect(r2: Recti) = {
		val r1 = this
		val lx = math.max(r1.x,r2.x)
		val ly = math.max(r1.y,r2.y)
		val hx = math.min(r1.x + r1.w,r2.x + r2.w)
		val hy = math.min(r1.y + r1.h,r2.y + r2.h)
		Recti(lx,ly,hx-lx,hy-ly)
	}

	def maxX = x + w
	def minX = x
	def maxY = y + h
	def minY = y

	def w = width
	def h = height

	def min = ReadVec2i(minX,minY)
	def max = ReadVec2i(maxX,maxY)

	def dimensions = ReadVec2i(width,height)
	def position = ReadVec2i(x,y)

	def xy = Vec2i(x,y)

	def axis(a : Axis) = a match {
		case Axis.X => RectiAxis(x,width)
		case Axis.Y => RectiAxis(y,height)
		case _ =>
			Noto.error(s"Cannot get non-xy axis from a rect, axis was $a")
			RectiAxis(0,0)
	}

	def setAxisTo(axis : Axis, values : RectiAxis) : Unit = {
		axis match {
			case Axis.X =>
				x = values.position
				width = values.length
			case Axis.Y =>
				y = values.position
				height = values.length
			case _ => // do nothing
		}
	}

	def withAxisSetTo(axis : Axis, values : RectiAxis) : Recti = {
		val r = Recti(x,y,width,height)
		r.setAxisTo(axis, values)
		r
	}
}


case class RectiAxis(var position : Int, var length : Int) {
}