package arx.game.data

import arx.engine.data.TWorldAuxData

class RandomizationWorldData extends TWorldAuxData with DefaultGameAuxData {
	var randomizationStyle : RandomizationStyle = RandomizationStyle.Random

	var seedOffset : Int = 0
}


sealed trait RandomizationStyle
object RandomizationStyle {
	case object Random extends RandomizationStyle
	case object Median extends RandomizationStyle
}