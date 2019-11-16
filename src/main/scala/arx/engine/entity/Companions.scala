package arx.engine.entity
import arx.core.introspection.Field
import arx.core.introspection.Clazz
object Companions {
import arx.engine.entity.IdentityData
object IdentityData extends Clazz[IdentityData]("IdentityData", classOf[IdentityData]){
	val Sentinel = new IdentityData
	override def instantiate = new IdentityData
	val name = Field.fromValue(Sentinel.name).createField[IdentityData]("name",f => f.name, (f,name) => f.name = name, IdentityData) 
	fields += "name" -> name
	val kind = Field.fromValue(Sentinel.kind).createField[IdentityData]("kind",f => f.kind, (f,kind) => f.kind = kind, IdentityData) 
	fields += "kind" -> kind

	def apply(f : IdentityData => Unit) : IdentityData = { val v = new IdentityData; f(v); v }
					 
	def copyInto(from : IdentityData, to : IdentityData) {
		to.name = from.name
		to.kind = from.kind
	}
}
}
