package arx.engine.graphics.data.windowing

import arx.application.Noto
import arx.core.datastructures.Watcher
import arx.core.macros.GenerateCompanion
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec4f
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.helpers.ConfigLoadingHelper
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.data.Moddable
import arx.engine.graphics.data.windowing.ImageDisplay.{PositionStyle, ScalingStyle}
import arx.graphics.helpers.{Color, RGBA}
import arx.graphics.{Axis, Image, ScaledImage, TToImage}
import arx.resource.ResourceManager

@GenerateCompanion
class ImageDisplay extends TWidgetAuxData {
	var image : Moddable[TToImage] = Moddable(ResourceManager.blankImage : TToImage)
	var scalingStyle : ScalingStyle = ImageDisplay.Scale(1.0f)
	var positionStyle : PositionStyle = ImageDisplay.TopLeft
	var color : Moddable[Color] = Moddable(Color.White)


	override def autoLoadSimpleValuesFromConfig: Boolean = false

	override def loadFromConfig(widget: Widget, configValue: ConfigValue, reload: Boolean): Unit = {
		for (cv <- configValue.fieldOpt("image")) {
			image = ConfigLoadingHelper.loadImageFromConfig(cv, widget)
		}
		for (ss <- configValue.fieldOpt("scalingStyle")) {
			scalingStyle = ScalingStyle.parse(ss.str, scalingStyle)
		}
		for (ps <- configValue.fieldOpt("positionStyle")) {
			positionStyle = PositionStyle.parse(ps.str, positionStyle)
		}
		for (fc <- ConfigLoadingHelper.loadColorFromConfig(configValue.color, widget)) {
			color = fc
		}
	}

	override def modificationSignature: AnyRef = (image.resolve(), color.resolve(), positionStyle, scalingStyle)
}


object ImageDisplay {
	sealed class ScalingStyle
	case object ScaleToFit extends ScalingStyle
	case class Scale(scaleFactor : Float) extends ScalingStyle
	case class ScaleToAxis(axis : Axis, size : Int) extends ScalingStyle

	private val scaleFractionPattern = "scale\\(([0-9.]+)\\)".r
	private val scalePercentPattern = "scale\\(([0-9]+)%\\)".r
	private val scaleToWidthPattern = "(?i)scale\\s?to\\s?width\\(?([0-9]+)px\\)?".r
	private val scaleToHeightPattern = "(?i)scale\\s?to\\s?height\\(?([0-9]+)px\\)?".r
	object ScalingStyle {
		def parse(str : String, orElse : ScalingStyle) = str.toLowerCase.replace(" ","") match {
			case "scaletofit" => ScaleToFit
			case scaleFractionPattern(fract) => Scale(fract.toFloat)
			case scalePercentPattern(pcnt) => Scale(pcnt.toFloat / 100.0f)
			case scaleToWidthPattern(width) => ScaleToAxis(Axis.X, width.toInt)
			case scaleToHeightPattern(height) => ScaleToAxis(Axis.Y, height.toInt)
			case "actualsize" => Scale(1.0f)
			case _ =>
				Noto.warn(s"unsupported scaling style: $str")
				orElse
		}
	}

	sealed class PositionStyle
	case object TopLeft extends PositionStyle
	case object Center extends PositionStyle
	object PositionStyle {
		def parse(str: String, orElse: PositionStyle): PositionStyle = str.toLowerCase.replace(" ","") match {
			case "topleft" => TopLeft
			case "center" | "centered" => Center
			case _ =>
				Noto.warn(s"unsupported position style: $str")
				orElse
		}
	}
}