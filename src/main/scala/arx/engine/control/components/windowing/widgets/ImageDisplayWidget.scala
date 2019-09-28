package arx.engine.control.components.windowing.widgets

import arx.engine.control.components.windowing.{Widget, WidgetConstructor, WidgetInstance, WidgetType, WindowingSystem}
import arx.engine.data.Moddable
import arx.engine.graphics.data.windowing.ImageDisplay
import arx.engine.graphics.data.windowing.ImageDisplay.{PositionStyle, ScalingStyle}
import arx.graphics.{Image, TToImage}

import scala.language.implicitConversions

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 11:26 AM
  */

object ImageDisplayWidget extends WidgetType[ImageDisplayWidget, ImageDisplay] {
	def apply(windowingSystem : WindowingSystem) : ImageDisplayWidget = {
		ImageDisplayWidget(windowingSystem, _ => {})
	}

	def apply(windowingSystem : WindowingSystem, iddInit : ImageDisplay => Unit) : ImageDisplayWidget = {
		initializeWidget(windowingSystem.createWidget())
	}

	def apply(windowingSystem : WindowingSystem, image : Image, positionStyle: PositionStyle = ImageDisplay.TopLeft, scalingStyle: ScalingStyle = ImageDisplay.Scale(1.0f)) : Widget = ImageDisplayWidget.apply(windowingSystem, idd => {
		idd.image = Moddable(image : TToImage)
		idd.positionStyle = positionStyle
		idd.scalingStyle = scalingStyle
	})

	override def initializeWidget(widget: Widget): ImageDisplayWidget = {
		widget.attachData[ImageDisplay]
		new ImageDisplayWidget(widget)
	}


	case class build(image : Image, positionStyle: PositionStyle = ImageDisplay.TopLeft, scalingStyle: ScalingStyle = ImageDisplay.Scale(1.0f)) extends WidgetConstructor[ImageDisplayWidget] {
		override def initializeWidget(widget: Widget): ImageDisplayWidget = {
			val w = ImageDisplayWidget.initializeWidget(widget)
			w.image = Moddable(image : TToImage)
			w.positionStyle = positionStyle
			w.scalingStyle = scalingStyle
			w
		}
	}
}


case class ImageDisplayWidget(widget : Widget) extends WidgetInstance {

}