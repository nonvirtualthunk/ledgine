package arx.engine.control.components.windowing.helpers

import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec4f
import arx.engine.control.components.windowing.Widget
import arx.engine.data.Moddable
import arx.graphics.helpers.{Color, RGBA}

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
			Some(Moddable(RGBA(configValue.v4)))
		}
	}
}
