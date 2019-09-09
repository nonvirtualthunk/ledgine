package arx.core.mat

import arx.core.vec._
@SerialVersionUID(9223372036854770000L)
class Mat4x3 extends ReadMat4x3{
	def this(m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m20i = m20a
		m30i = m30a
		m01i = m01a
		m11i = m11a
		m21i = m21a
		m31i = m31a
		m02i = m02a
		m12i = m12a
		m22i = m22a
		m32i = m32a
	}
	override def m00_= ( s : Float ) { m00i = s }
	override def m10_= ( s : Float ) { m10i = s }
	override def m20_= ( s : Float ) { m20i = s }
	override def m30_= ( s : Float ) { m30i = s }
	override def m01_= ( s : Float ) { m01i = s }
	override def m11_= ( s : Float ) { m11i = s }
	override def m21_= ( s : Float ) { m21i = s }
	override def m31_= ( s : Float ) { m31i = s }
	override def m02_= ( s : Float ) { m02i = s }
	override def m12_= ( s : Float ) { m12i = s }
	override def m22_= ( s : Float ) { m22i = s }
	override def m32_= ( s : Float ) { m32i = s }
	def +=(v : ReadMat4x3) { m00i += v.m00;m10i += v.m10;m20i += v.m20;m30i += v.m30;m01i += v.m01;m11i += v.m11;m21i += v.m21;m31i += v.m31;m02i += v.m02;m12i += v.m12;m22i += v.m22;m32i += v.m32}
	def -=(v : ReadMat4x3) { m00i -= v.m00;m10i -= v.m10;m20i -= v.m20;m30i -= v.m30;m01i -= v.m01;m11i -= v.m11;m21i -= v.m21;m31i -= v.m31;m02i -= v.m02;m12i -= v.m12;m22i -= v.m22;m32i -= v.m32}
	override def +(v : ReadMat4x3) = new Mat4x3(m00i + v.m00,m10i + v.m10,m20i + v.m20,m30i + v.m30,m01i + v.m01,m11i + v.m11,m21i + v.m21,m31i + v.m31,m02i + v.m02,m12i + v.m12,m22i + v.m22,m32i + v.m32)
	override def -(v : ReadMat4x3) = new Mat4x3(m00i - v.m00,m10i - v.m10,m20i - v.m20,m30i - v.m30,m01i - v.m01,m11i - v.m11,m21i - v.m21,m31i - v.m31,m02i - v.m02,m12i - v.m12,m22i - v.m22,m32i - v.m32)
	def +=(s : Float) {m00i += s;m10i += s;m20i += s;m30i += s;m01i += s;m11i += s;m21i += s;m31i += s;m02i += s;m12i += s;m22i += s;m32i += s}
	def -=(s : Float) {m00i -= s;m10i -= s;m20i -= s;m30i -= s;m01i -= s;m11i -= s;m21i -= s;m31i -= s;m02i -= s;m12i -= s;m22i -= s;m32i -= s}
	def *=(s : Float) {m00i *= s;m10i *= s;m20i *= s;m30i *= s;m01i *= s;m11i *= s;m21i *= s;m31i *= s;m02i *= s;m12i *= s;m22i *= s;m32i *= s}
	def /=(s : Float) {m00i /= s;m10i /= s;m20i /= s;m30i /= s;m01i /= s;m11i /= s;m21i /= s;m31i /= s;m02i /= s;m12i /= s;m22i /= s;m32i /= s}
	override def +(s : Float) = new Mat4x3(m00i + s,m10i + s,m20i + s,m30i + s,m01i + s,m11i + s,m21i + s,m31i + s,m02i + s,m12i + s,m22i + s,m32i + s)
	override def -(s : Float) = new Mat4x3(m00i - s,m10i - s,m20i - s,m30i - s,m01i - s,m11i - s,m21i - s,m31i - s,m02i - s,m12i - s,m22i - s,m32i - s)
	override def *(s : Float) = new Mat4x3(m00i * s,m10i * s,m20i * s,m30i * s,m01i * s,m11i * s,m21i * s,m31i * s,m02i * s,m12i * s,m22i * s,m32i * s)
	override def /(s : Float) = new Mat4x3(m00i / s,m10i / s,m20i / s,m30i / s,m01i / s,m11i / s,m21i / s,m31i / s,m02i / s,m12i / s,m22i / s,m32i / s)

}
object Mat4x3{
	def apply (m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float) = new Mat4x3(m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float)
	def apply (v : ReadMat4x3) = new Mat4x3(v.m00,v.m10,v.m20,v.m30,v.m01,v.m11,v.m21,v.m31,v.m02,v.m12,v.m22,v.m32)
	implicit def toWriteable (v : ReadMat4x3) = new Mat4x3(v.m00,v.m10,v.m20,v.m30,v.m01,v.m11,v.m21,v.m31,v.m02,v.m12,v.m22,v.m32)
}
