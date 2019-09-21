package arx.engine.graphics.data.windowing

import arx.core.datastructures.Watcher
import arx.core.vec.ReadVec4f
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.data.Moddable
import arx.engine.graphics.data.windowing.ImageDisplay.{PositionStyle, ScalingStyle}
import arx.graphics.helpers.{Color, RGBA}
import arx.graphics.{Image, TToImage}
import arx.resource.ResourceManager

class ImageDisplay extends TWidgetAuxData {
	var image : Moddable[TToImage] = Moddable(ResourceManager.blankImage : TToImage)
	var scalingStyle : ScalingStyle = ImageDisplay.ActualSize(1.0f)
	var positionStyle : PositionStyle = ImageDisplay.TopLeft
	var color : Color = Color.White

	protected[engine] var watcher = Watcher(image.resolve())
}


object ImageDisplay {
	sealed class ScalingStyle
	case object ScaleToFit extends ScalingStyle
	case class ActualSize(scaleFactor : Float) extends ScalingStyle

	sealed class PositionStyle
	case object TopLeft extends PositionStyle
	case object Center extends PositionStyle
}