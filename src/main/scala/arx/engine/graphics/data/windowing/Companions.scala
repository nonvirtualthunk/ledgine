package arx.engine.graphics.data.windowing
import arx.core.introspection.Field
import arx.core.introspection.Clazz
object Companions {
import arx.engine.graphics.data.windowing.ImageDisplay
object ImageDisplay extends Clazz[ImageDisplay]("ImageDisplay", classOf[ImageDisplay]){
	val Sentinel = new ImageDisplay
	override def instantiate = new ImageDisplay
	val image = Field.fromValue(Sentinel.image).createField[ImageDisplay]("image",f => f.image, (f,image) => f.image = image, ImageDisplay) 
	fields += "image" -> image
	val scalingStyle = Field.fromValue(Sentinel.scalingStyle).createField[ImageDisplay]("scalingStyle",f => f.scalingStyle, (f,scalingStyle) => f.scalingStyle = scalingStyle, ImageDisplay) 
	fields += "scalingStyle" -> scalingStyle
	val positionStyle = Field.fromValue(Sentinel.positionStyle).createField[ImageDisplay]("positionStyle",f => f.positionStyle, (f,positionStyle) => f.positionStyle = positionStyle, ImageDisplay) 
	fields += "positionStyle" -> positionStyle
	val color = Field.fromValue(Sentinel.color).createField[ImageDisplay]("color",f => f.color, (f,color) => f.color = color, ImageDisplay) 
	fields += "color" -> color

	def apply(f : ImageDisplay => Unit) : ImageDisplay = { val v = new ImageDisplay; f(v); v }
					 
	def copyInto(from : ImageDisplay, to : ImageDisplay) {
		to.image = from.image
		to.scalingStyle = from.scalingStyle
		to.positionStyle = from.positionStyle
		to.color = from.color
	}
}
}
