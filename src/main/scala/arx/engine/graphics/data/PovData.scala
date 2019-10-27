package arx.engine.graphics.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.data.{TMutableAuxData, TWorldAuxData}
import arx.graphics.pov.EyeCamera
import arx.graphics.pov.TCamera



class PovData extends TGraphicsData with TWorldAuxData with TMutableAuxData {
	var pov : TCamera = new EyeCamera()
}
