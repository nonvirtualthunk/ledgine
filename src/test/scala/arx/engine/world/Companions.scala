package arx.engine.world
import arx.core.introspection.Field
import arx.core.introspection.Clazz
object Companions {
import arx.engine.world.HypotheticalWorldTest.TestCreatureData
object TestCreatureData extends Clazz[TestCreatureData]("TestCreatureData", classOf[TestCreatureData]){
	val Sentinel = new TestCreatureData
	val hp = Field.fromValue(Sentinel.hp).createField[TestCreatureData]("hp",f => f.hp, (f,hp) => f.hp = hp, TestCreatureData) 
	fields += "hp" -> hp
	val size = Field.fromValue(Sentinel.size).createField[TestCreatureData]("size",f => f.size, (f,size) => f.size = size, TestCreatureData) 
	fields += "size" -> size

	def apply(f : TestCreatureData => Unit) : TestCreatureData = { val v = new TestCreatureData; f(v); v }

	override def copyInto(from: TestCreatureData, to: TestCreatureData): Unit = {
		to.hp = from.hp
		to.size = from.size
	}
}
import arx.engine.world.HypotheticalWorldTest.TestAttackData
object TestAttackData extends Clazz[TestAttackData]("TestAttackData", classOf[TestAttackData]){
	val Sentinel = new TestAttackData
	val toHit = Field.fromValue(Sentinel.toHit).createField[TestAttackData]("toHit",f => f.toHit, (f,toHit) => f.toHit = toHit, TestAttackData) 
	fields += "toHit" -> toHit
	val strikeCount = Field.fromValue(Sentinel.strikeCount).createField[TestAttackData]("strikeCount",f => f.strikeCount, (f,strikeCount) => f.strikeCount = strikeCount, TestAttackData) 
	fields += "strikeCount" -> strikeCount
	val strikeDamage = Field.fromValue(Sentinel.strikeDamage).createField[TestAttackData]("strikeDamage",f => f.strikeDamage, (f,strikeDamage) => f.strikeDamage = strikeDamage, TestAttackData) 
	fields += "strikeDamage" -> strikeDamage

	def apply(f : TestAttackData => Unit) : TestAttackData = { val v = new TestAttackData; f(v); v }

	override def copyInto(from: TestAttackData, to: TestAttackData): Unit = {
		to.toHit = from.toHit
		to.strikeCount = from.strikeCount
		to.strikeDamage = from.strikeDamage
	}
}
}
