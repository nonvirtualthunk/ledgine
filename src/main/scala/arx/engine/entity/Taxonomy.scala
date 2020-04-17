package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/19/18
 * Time: 7:03 AM
 */

import arx.Prelude.{toArxString, toRichTimer}
import arx.application.Noto
import arx.core.introspection.ReflectionAssistant
import arx.core.macros.GenerateCompanion
import arx.core.metrics.Metrics
import arx.core.representation.ConfigValue
import arx.engine.data.{CustomConfigDataLoader, TAuxData}
import arx.resource.ResourceManager


object Taxonomy {
	protected var taxonsByName = Map[String, Set[Taxon]]()
	protected var taxonsByNameAndNamespace = Map[(String, String), Taxon]()

	protected def createTaxon(name: String, namespace: String, parents: Taxon*): Taxon = createTaxonL(name, namespace, parents.toList)

	protected def createTaxonL(name: String, namespace: String, parents: List[Taxon]): Taxon = {
		val stdName = standardizeTaxonString(name)
		val t = taxonsByNameAndNamespace.getOrElse(name -> namespace, Taxon(stdName, standardizeTaxonString(namespace), parents))
		if (t.parents != parents) {
			Noto.error(s"Two taxons with identical name.namespace have different parents: $name, $namespace")
		}
		taxonsByNameAndNamespace += (name, namespace) -> t
		taxonsByName += stdName -> (taxonsByName.getOrElse(stdName, Set()) + t)
		t
	}

	def standardizeTaxonString(str: String) = str.fromCamelCase.toLowerCase

	def getByName(name: String): Option[Taxon] = taxonsByName.get(standardizeTaxonString(name)).flatMap(_.headOption)

	/**
	 * Retrieves a taxon with the given name, starting looking from the provided namespace and working backwards until a taxon is found (currently jumps straight to checking base namespace)
	 */
	def getByName(name: String, namespace: String): Option[Taxon] = taxonsByNameAndNamespace.get(standardizeTaxonString(name) -> standardizeTaxonString(namespace)).orElse(taxonsByNameAndNamespace.get(standardizeTaxonString(name), ""))

	def apply(name : String) : Taxon = byNameExpr(name)
	def apply(name: String, namespace: String): Taxon = taxon(name, namespace)

	def byNameExpr(nameExpr : String) = if (nameExpr.contains('.')) {
		val splitIndex = nameExpr.lastIndexOf('.')
		val (namespace, name) = nameExpr.splitAt(splitIndex+1)
		taxon(name, namespace.dropRight(1))
	} else {
		taxon(nameExpr)
	}

	def taxon(name : String) : Taxon = getByName(name).getOrElse {
		Noto.warn(s"expected to get existing taxon with name $name but none found")
		Taxon(name.fromCamelCase.toLowerCase, "", Taxon("unknown thing", ""))
	}
	def taxon(name: String, namespace: String): Taxon = {
		getByName(name, namespace).getOrElse {
			Noto.warn(s"expected to get existing taxon with name $name but none found")
			Taxon(name.fromCamelCase.toLowerCase, "", Taxon("unknown thing", ""))
		}
	}

	def descendantsOf(name: String): List[Taxon] = {
		getByName(name) match {
			case Some(root) => taxonsByNameAndNamespace.values.filter(_.isA(root)).flatMap(t => selfAndDescendantsOf(t)).toList
			case None => List()
		}
	}

	def selfAndDescendantsOf(taxon: Taxon): List[Taxon] = {
		selfAndDescendantsOfIntern(taxon).distinct
	}
	def selfAndDescendantsOfIntern(taxon: Taxon): List[Taxon] = {
		taxon :: taxonsByNameAndNamespace.values.filter(t => t.isA(taxon) && t != taxon).flatMap(t => selfAndDescendantsOfIntern(t)).toList
	}

	def load(config: ConfigValue): Unit = Metrics.timer("Taxonomy.load").timeStmt {
		var taxonParentPairs: Map[(String, String), List[String]] = Map()

		def loadFromObj(obj: ConfigValue, namespace: String): Unit = {
			for ((name, value) <- obj.fields) {
				if (value.isObj) {
					val newNamespace = if (namespace.isEmpty) {
						name
					} else {
						namespace + "." + standardizeTaxonString(name)
					}
					loadFromObj(value, newNamespace)
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
					taxonParentPairs += (standardizeTaxonString(name), standardizeTaxonString(namespace)) -> parents.map(standardizeTaxonString)
				}
			}
		}

		def addTaxon(t: (String, String)): Taxon = {
			val (name, namespace) = t
			val parents = taxonParentPairs(t)
			val parentTaxons = parents.map(p => {
				var found : Option[Taxon] = None
				var tmpNamespace = namespace
				while (found.isEmpty) {
					found = getByName(p, tmpNamespace)
					if (tmpNamespace.contains('.')) {
						tmpNamespace = tmpNamespace.substring(0, tmpNamespace.lastIndexOf('.'))
					} else if (tmpNamespace.nonEmpty) {
						tmpNamespace = ""
					} else {
						Noto.error(s"Taxon referenced non-present parent: $namespace.$name -> $namespace.$p")
						found = Some(addTaxon((p, namespace)))
					}
				}
				found.get
			})
			taxonParentPairs -= t
			val ret = getByName(name, namespace).get
			ret.parents = parentTaxons
			ret
		}

		for (taxonConf <- config.fieldOpt("Taxonomy")) {
			loadFromObj(taxonConf, "")

			for ((name, namespace) <- taxonParentPairs.keys) {
				createTaxonL(name, namespace, Nil)
			}

			while (taxonParentPairs.nonEmpty) {
				addTaxon(taxonParentPairs.keys.head)
			}
		}
	}

	def loadAll(): Unit = {
		load(ResourceManager.sml("game/data/Taxonomy.sml"))
	}

	val UnknownThing = createTaxon("unknown thing", "")


	loadAll()
}

@GenerateCompanion
class IdentityData extends TAuxData {
	def this(kind: Taxon) {
		this()
		this.kind = kind
	}

	def this(nomen: String, kind: Taxon) {
		this()
		name = Some(nomen)
		this.kind = kind
	}

	var name: Option[String] = None

	def name_=(s: String): Unit = {
		name = Some(s)
	}

	var kind: Taxon = Taxonomy.UnknownThing

	def isA(taxon: Taxon) = this.kind.isA(taxon)
}