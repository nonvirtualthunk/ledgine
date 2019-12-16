package arx.game.logic

import arx.engine.world.World
import arx.game.data.DicePoolBuilder._
import arx.game.data.{DiceRoll, RandomizationStyle, RandomizationWorldData}
import arx.game.logic.Randomizer.stdDicePool

import scala.util.Random

object Randomizer {
	val stdDicePool = 3 d 6

	def apply(implicit world : World) = {
		val randomization = world.view.worldData[RandomizationWorldData]
		new Randomizer(new Random(world.currentTime.time + randomization.seedOffset * 13), randomization.randomizationStyle)
	}
}


class Randomizer(private val random : Random, val style : RandomizationStyle) {
	/**
	 * Standard-ish distribution between [3,18]
	 */
	def stdRoll() = {
		style match {
			case RandomizationStyle.Random => Randomizer.stdDicePool.roll()(this)
			case RandomizationStyle.Median => DiceRoll(List(3,4,3))
		}
	}

	def nextInt(maxExclusive : Int) = {
		style match {
			case RandomizationStyle.Random => if (maxExclusive == 0) { 0 } else { random.nextInt(maxExclusive) }
			case RandomizationStyle.Median => maxExclusive / 2
		}
	}
}