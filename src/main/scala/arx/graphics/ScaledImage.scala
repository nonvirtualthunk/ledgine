package arx.graphics

import arx.core.vec.{ReadVec2f, Vec2f}

case class ScaledImage(image : Image, scale : ReadVec2f) {

}

object ScaledImage {
	def scaleToPixelWidth(image : Image, pixelWidth : Int) = {
		val fract = pixelWidth.toFloat / image.width.toFloat
		ScaledImage(image, Vec2f(fract, fract))
	}
}