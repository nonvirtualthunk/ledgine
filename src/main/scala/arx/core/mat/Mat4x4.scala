package arx.core.mat

import arx.core.vec._
@SerialVersionUID(9223372036854770000L)
class Mat4x4 extends ReadMat4x4{
	def this(m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float,m03a : Float,m13a : Float,m23a : Float,m33a : Float){ 
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
		m03i = m03a
		m13i = m13a
		m23i = m23a
		m33i = m33a
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
	override def m03_= ( s : Float ) { m03i = s }
	override def m13_= ( s : Float ) { m13i = s }
	override def m23_= ( s : Float ) { m23i = s }
	override def m33_= ( s : Float ) { m33i = s }
	def +=(v : ReadMat4x4) { m00i += v.m00;m10i += v.m10;m20i += v.m20;m30i += v.m30;m01i += v.m01;m11i += v.m11;m21i += v.m21;m31i += v.m31;m02i += v.m02;m12i += v.m12;m22i += v.m22;m32i += v.m32;m03i += v.m03;m13i += v.m13;m23i += v.m23;m33i += v.m33}
	def -=(v : ReadMat4x4) { m00i -= v.m00;m10i -= v.m10;m20i -= v.m20;m30i -= v.m30;m01i -= v.m01;m11i -= v.m11;m21i -= v.m21;m31i -= v.m31;m02i -= v.m02;m12i -= v.m12;m22i -= v.m22;m32i -= v.m32;m03i -= v.m03;m13i -= v.m13;m23i -= v.m23;m33i -= v.m33}
	override def +(v : ReadMat4x4) = new Mat4x4(m00i + v.m00,m10i + v.m10,m20i + v.m20,m30i + v.m30,m01i + v.m01,m11i + v.m11,m21i + v.m21,m31i + v.m31,m02i + v.m02,m12i + v.m12,m22i + v.m22,m32i + v.m32,m03i + v.m03,m13i + v.m13,m23i + v.m23,m33i + v.m33)
	override def -(v : ReadMat4x4) = new Mat4x4(m00i - v.m00,m10i - v.m10,m20i - v.m20,m30i - v.m30,m01i - v.m01,m11i - v.m11,m21i - v.m21,m31i - v.m31,m02i - v.m02,m12i - v.m12,m22i - v.m22,m32i - v.m32,m03i - v.m03,m13i - v.m13,m23i - v.m23,m33i - v.m33)
	def +=(s : Float) {m00i += s;m10i += s;m20i += s;m30i += s;m01i += s;m11i += s;m21i += s;m31i += s;m02i += s;m12i += s;m22i += s;m32i += s;m03i += s;m13i += s;m23i += s;m33i += s}
	def -=(s : Float) {m00i -= s;m10i -= s;m20i -= s;m30i -= s;m01i -= s;m11i -= s;m21i -= s;m31i -= s;m02i -= s;m12i -= s;m22i -= s;m32i -= s;m03i -= s;m13i -= s;m23i -= s;m33i -= s}
	def *=(s : Float) {m00i *= s;m10i *= s;m20i *= s;m30i *= s;m01i *= s;m11i *= s;m21i *= s;m31i *= s;m02i *= s;m12i *= s;m22i *= s;m32i *= s;m03i *= s;m13i *= s;m23i *= s;m33i *= s}
	def /=(s : Float) {m00i /= s;m10i /= s;m20i /= s;m30i /= s;m01i /= s;m11i /= s;m21i /= s;m31i /= s;m02i /= s;m12i /= s;m22i /= s;m32i /= s;m03i /= s;m13i /= s;m23i /= s;m33i /= s}
	override def +(s : Float) = new Mat4x4(m00i + s,m10i + s,m20i + s,m30i + s,m01i + s,m11i + s,m21i + s,m31i + s,m02i + s,m12i + s,m22i + s,m32i + s,m03i + s,m13i + s,m23i + s,m33i + s)
	override def -(s : Float) = new Mat4x4(m00i - s,m10i - s,m20i - s,m30i - s,m01i - s,m11i - s,m21i - s,m31i - s,m02i - s,m12i - s,m22i - s,m32i - s,m03i - s,m13i - s,m23i - s,m33i - s)
	override def *(s : Float) = new Mat4x4(m00i * s,m10i * s,m20i * s,m30i * s,m01i * s,m11i * s,m21i * s,m31i * s,m02i * s,m12i * s,m22i * s,m32i * s,m03i * s,m13i * s,m23i * s,m33i * s)
	override def /(s : Float) = new Mat4x4(m00i / s,m10i / s,m20i / s,m30i / s,m01i / s,m11i / s,m21i / s,m31i / s,m02i / s,m12i / s,m22i / s,m32i / s,m03i / s,m13i / s,m23i / s,m33i / s)

}
object Mat4x4{
	def apply (m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float,m03a : Float,m13a : Float,m23a : Float,m33a : Float) = new Mat4x4(m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float,m03a : Float,m13a : Float,m23a : Float,m33a : Float)
	def apply (v : ReadMat4x4) = new Mat4x4(v.m00,v.m10,v.m20,v.m30,v.m01,v.m11,v.m21,v.m31,v.m02,v.m12,v.m22,v.m32,v.m03,v.m13,v.m23,v.m33)
	implicit def toWriteable (v : ReadMat4x4) = new Mat4x4(v.m00,v.m10,v.m20,v.m30,v.m01,v.m11,v.m21,v.m31,v.m02,v.m12,v.m22,v.m32,v.m03,v.m13,v.m23,v.m33)
	def apply (s : Float) = new Mat4x4(s,0,0,0,0,s,0,0,0,0,s,0,0,0,0,s)
	object _Identity extends ReadMat4x4(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1) {
		override def * ( v : ReadMat4x4 ) = v
		def * ( v : Mat4x4 ) = v
	}
	val Identity : ReadMat4x4 = _Identity
}
