package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/19/18
 * Time: 7:03 AM
 */

import arx.Prelude.{toArxString, toRichTimer}
import arx.application.Noto
import arx.core.macros.GenerateCompanion
import arx.core.metrics.Metrics
import arx.core.representation.ConfigValue
import arx.engine.data.TAuxData
import arx.resource.ResourceManager


object Taxonomy {
	protected var taxonsByName = Map[String,Taxon]()
	protected def createTaxon(name : String, parents : Taxon*) : Taxon = createTaxonL(name, parents.toList)
	protected def createTaxonL(name : String, parents : List[Taxon]) : Taxon = {
		taxonsByName.get(name) match {
			case Some(t) => if (t.parents == parents) {
				t
			} else {
				Noto.error(s"Taxon $name created with two incompatible sets of parents: ${t.parents}, $parents")
				t
			}
			case None =>
				val t = Taxon(name, parents)
				taxonsByName += standardizeStr(name.fromCamelCase) -> t
				t
		}
	}
	protected def standardizeStr(str : String) = str.fromCamelCase.toLowerCase
	def getByName(name : String) : Option[Taxon] = taxonsByName.get(standardizeStr(name))
	def apply(name : String) : Taxon = taxon(name)
	def taxon(name : String) : Taxon = getByName(name).getOrElse {
		Noto.warn(s"expected to get existing taxon with name $name but none found")
		Taxon(name.fromCamelCase.toLowerCase, Taxon("unknown thing"))
	}

	def descendantsOf(name : String) : List[Taxon] = {
		getByName(name) match {
			case Some(root) => taxonsByName.values.filter(_.isA(root)).flatMap(t => selfAndDescendantsOf(t)).toList
			case None => List()
		}
	}
	def selfAndDescendantsOf(taxon : Taxon) : List[Taxon] = {
		taxon :: taxonsByName.values.filter(t => t.isA(taxon) && t != taxon).flatMap(t => selfAndDescendantsOf(t)).toList
	}

	def load(config : ConfigValue): Unit = Metrics.timer("Taxonomy.load").timeStmt {
		var taxonParentPairs : Map[String,List[String]] = Map()

		def loadFromObj(obj : ConfigValue): Unit = {
			for ((name, value) <- obj.fields) {
				if (value.isObj) {
					loadFromObj(value)
				} else {
					val parents = if (value.isArr) {
						value.arr.map(c => c.str).toList
					} else {
						List(value.str)
					}
//					val parents = if (value.isArr) {
//						value.arr.map(c => c.str -> getByName(c.str))
//					} else {
//						List(value.str -> getByName(value.str))
//					}
//					parents.foreach(p => if (p._2.isEmpty) { Noto.warn(s"could not resolve parent taxon : ${p._1}")})
//					createTaxonL(name.fromCamelCase.toLowerCase, parents.flatMap(_._2).toList)
					taxonParentPairs += standardizeStr(name) -> parents.map(standardizeStr)
				}
			}
		}

		def addTaxon(t : String): Taxon = {
			val parents = taxonParentPairs(t)
			val parentTaxons = parents.map(p => {
				getByName(p) match {
					case Some(existing) => existing
					case None => addTaxon(p)
				}
			})
			taxonParentPairs -= t
			createTaxonL(t, parentTaxons)
		}
		for (taxonConf <- config.fieldOpt("Taxonomy")) {
			loadFromObj(taxonConf)

			while (taxonParentPairs.nonEmpty) {
				addTaxon(taxonParentPairs.keys.head)
			}
		}
	}

	def loadAll(): Unit = {
		load(ResourceManager.sml("game/data/Taxonomy.sml"))
	}

	val UnknownThing = createTaxon("unknown thing")


	loadAll()
//
//
//	protected trait SubTaxonomy {
//		private var taxonsByName = Map[String,Taxon]()
//		protected def createTaxon(name : String, parents : Taxon*) : Taxon = {
//			val t = Taxonomy.createTaxon(name, parents : _*)
//			taxonsByName += name.toLowerCase -> t
//			t
//		}
//
//		def byName(name : String) : Option[Taxon] = taxonsByName.get(name.toLowerCase).orElse(Taxonomy.taxonsByName.get(name.toLowerCase))
//	}
//
//
//	val UnknownThing = createTaxon("unknown thing")
//
//	val Material = createTaxon("material")
//
//	object Materials extends SubTaxonomy {
//		val Wood = createTaxon("wood", Material)
//		val Stone = createTaxon("stone", Material)
//		val Metal = createTaxon("metal", Material)
//	}
//
//	val LivingThing = createTaxon("living thing")
//	val Creature = createTaxon("creature", LivingThing)
//	val Monster = createTaxon("monster", Creature)
//
//	object Creatures extends SubTaxonomy {
//		val Human = createTaxon("human", Creature)
//
//		val MudMonster = createTaxon("mud monster", Monster)
//	}
//
//	val Item = createTaxon("item")
//	val Weapon = createTaxon("weapon", Item)
//	val Axe = createTaxon("axe", Item)
//
//	object Weapons extends SubTaxonomy {
//		val BattleAxe = createTaxon("battleaxe", Weapon, Axe)
//
//		val Sword = createTaxon("sword", Weapon)
//
//		val Longsword = createTaxon("longsword", Weapon, Sword)
//		val Shortsword = createTaxon("shortsword", Weapon, Sword)
//	}
//
//	val AttackType = createTaxon("attack type")
//	object AttackTypes extends SubTaxonomy {
//		val PhysicalAttack = createTaxon("physical attack", AttackType)
//		val SlashingAttack = createTaxon("slashing attack", PhysicalAttack)
//		val StabbingAttack = createTaxon("stabbing attack", PhysicalAttack)
//
//		val NaturalAttack = createTaxon("natural attack", AttackType)
//
//		val MeleeAttack = createTaxon("melee attack", AttackType)
//		val RangedAttack = createTaxon("ranged attack", AttackType)
//		val ReachAttack = createTaxon("reach attack", MeleeAttack)
//
//
//	}
//
//	val Terrain = createTaxon("terrain")
//
//	object Terrains extends SubTaxonomy {
//		val Flatland = createTaxon("flatland", Terrain)
//		val Hills = createTaxon("hills", Terrain)
//		val Mountains = createTaxon("mountains", Terrain)
//	}
//
//	val Vegetation = createTaxon("vegetation")
//
//	object Vegetations extends SubTaxonomy {
//		val Grass = createTaxon("grass", Vegetation)
//		val Forest = createTaxon("forest", Vegetation)
//		val DeciduousForest = createTaxon("deciduous forest", Forest)
//		val EvergreenForest = createTaxon("evergreen forest", Forest)
//	}
//
//	val SpeciesRoot = createTaxon("species")
//
//	object Species extends SubTaxonomy {
//		val Humanoid = createTaxon("humanoid", SpeciesRoot)
//		val Monstrous = createTaxon("monstrous", SpeciesRoot)
//	}
//
//	val Skill = createTaxon("skill")
//
//	object Skills extends SubTaxonomy {
//		val CombatSkill = createTaxon("combat skill", Skill)
//		val WeaponSkill = createTaxon("weapon skill", CombatSkill)
//		val MagicSkill = createTaxon("magic skill", Skill)
//		val CraftingSkill = createTaxon("crafting skill", Skill)
//		val MovementSkill = createTaxon("movement skill", Skill)
//		val SurvivalSkill = createTaxon("survival skill", Skill)
//		val GatheringSkill = createTaxon("gathering skill", Skill)
//	}
//
//	val CharacterClass = createTaxon("character class")
//	object CharacterClasses extends SubTaxonomy {
//		val CombatClass = createTaxon("combat class", CharacterClass)
//		val MeleeCombatClass = createTaxon("melee combat class", CombatClass)
//		val RangedCombatClass = createTaxon("ranged combat class", CombatClass)
//		val MagicClass = createTaxon("magic class", CharacterClass)
//
//
//	}
//
//
//	val Action = createTaxon("action")
//	object Actions extends SubTaxonomy {
//		val MoveAction = createTaxon("move", Action)
//		val AttackAction = createTaxon("attack", Action)
//		val GatherAction = createTaxon("gather", Action)
//		val SwitchActiveCharacterAction = createTaxon("switch active", Action)
//	}

}

@GenerateCompanion
class IdentityData extends TAuxData {
	def this(kind : Taxon) {
		this()
		this.kind = kind
	}
	def this(nomen : String, kind : Taxon) {
		this()
		name = Some(nomen)
		this.kind = kind
	}
	var name : Option[String] = None
	def name_=(s : String): Unit = {
		name = Some(s)
	}
	var kind: Taxon = Taxonomy.UnknownThing

	def isA(taxon : Taxon) = this.kind.isA(taxon)
}