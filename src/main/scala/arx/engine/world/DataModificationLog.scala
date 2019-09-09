package arx.engine.world

import arx.core.introspection.Field
import arx.core.traits.ArxGeneric
import arx.engine.data.TAuxData

class DataModificationLog[C <: TAuxData](val baseValue : C, val finalValue : C, val fieldBreakdowns : Map[Field[_,_], Breakdown[Any]]) {
	def breakdownFor[T](field : Field[C,T], baseSource : String) : Breakdown[T] = {
		val baseBreakdown = fieldBreakdowns.get(field) match {
			case Some(breakdown) => breakdown.asInstanceOf[Breakdown[T]]
			case None => Breakdown(field.getter(finalValue), Vector())
		}

		val baseFieldValue = field.getter(baseValue)
		// if the base value is not conceptually empty (0, 0.0f, false, empty set, etc) then add an explicit reference to the base value
		if (!ArxGeneric.isConceptuallyEmpty(baseFieldValue)) {
			Breakdown(baseBreakdown.total, BreakdownElement[T](Some(baseSource), s"$baseFieldValue", Impact.Neutral) +: baseBreakdown.elements)
		} else {
			baseBreakdown
		}
	}
}