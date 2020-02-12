package arx.engine.control.components.windowing.helpers

import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec4f
import arx.engine.control.components.windowing.Widget
import arx.engine.data.Moddable
import arx.graphics.{Image, ScaledImage, TToImage}
import arx.graphics.helpers.{Color, RGBA}
import arx.resource.ResourceManager

object ConfigLoadingHelper {

	def loadColorFromConfig(configValue : ConfigValue, widget : Widget) : Option[Moddable[Color]] = {
		if (configValue.isSentinel) {
			None
		} else if (configValue.isStr) {
			configValue.str match {
				case Widget.bindingParser(binding) =>
					Some(Moddable(() => widget.resolveBinding(binding) match {
						case Some(boundValue) => boundValue match {
							case color: Color => color
							case v: ReadVec4f => RGBA(v)
							case other =>
								Noto.warn(s"invalid bound value for an image display color : $other")
								Color.White
						}
						case None => Color.White
					}))
				case _ =>
					None
			}
		} else {
			val v = configValue.v4
			val rgba = if (v.r > 1.9f || v.g > 1.9f || v.b > 1.9f || v.a > 1.9f) {
				RGBA(v / 255.0f)
			} else {
				RGBA(v)
			}
			Some(Moddable(rgba))
		}
	}

	def loadBooleanFromConfig(config : ConfigValue, widget : Widget, defaultValue : Boolean) : Moddable[Boolean] = {
		val str = config.str
		Widget.bindingParser.findFirstMatchIn(str) match {
			case Some(matched) => Moddable(() => widget.resolveBinding(matched.group(1)) match {
				case Some(boundValue) => boundValue match {
					case b: Boolean => b
					case other =>
						Noto.warn(s"Invalid bound value for a boolean : $other")
						defaultValue
				}
				case None => defaultValue
			})
			case None => Moddable(config.bool)
		}
	}

	def loadImageFromConfig(config : ConfigValue, widget : Widget) : Moddable[TToImage] = {
		val str = config.str
		Widget.bindingParser.findFirstMatchIn(str) match {
			case Some(matched) => Moddable(() => widget.resolveBinding(matched.group(1)) match {
				case Some(boundValue) => boundValue match {
					case img : Image => img
					case scaledImage : ScaledImage => scaledImage.image
					case str : String => ResourceManager.image(str)
					case timg: TToImage => timg.image
					case None => ResourceManager.emptyImage
					case other =>
						Noto.warn(s"invalid bound value for an image display : $other")
						ResourceManager.blankImage
				}
				case None => ResourceManager.blankImage
			})
			case None => Moddable(str : TToImage)
		}
	}
}
