package arx.engine.entity

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/16/18
  * Time: 8:26 AM
  */

import java.util.Objects

import arx.Prelude._
import arx.core.vec._

class Taxon(val name : String, val namespace : String, protected var _parents : List[Taxon]) {
	def parents = _parents
	protected[entity] def parents_=(p : List[Taxon]) { _parents = p }

	def isA(other : Taxon) : Boolean = {
		if (other == this) {
			true
		} else {
			parents.exists(t => t.isA(other))
		}
	}

	override def equals(other : Any) : Boolean = {
		other match {
			case t : Taxon => t.name == this.name && t.namespace == this.namespace
			case _ => false
		}
	}

	/**
	 * Gets a list consisting of this taxon and all of its ancestors, excluding any branch containing the given limit
	 */
	def selfAndAncestorsUpTo(limit : Taxon) : List[Taxon] = this :: parents.filter(_ != limit).flatMap(_.selfAndAncestorsUpTo(limit))

	import arx.Prelude._
	def displayName = {
		name.fromCamelCase.capitalizeAll
	}

	override val hashCode = (namespace, name).hashCode()

	override def toString: String = name
}
object Taxon {
	def apply(name : String, namespace : String, parents : Taxon*) : Taxon = {
		new Taxon(name, namespace, parents.toList)
	}

	def apply(name : String, namespace : String, parents : List[Taxon]) : Taxon = {
		new Taxon(name, namespace, parents)
	}

}

class Identity(val taxons : List[Taxon]) {
	def this(taxons_ : Taxon*) {
		this(taxons_.toList)
	}
	def isA(x : Taxon) = taxons.exists(t => t.isA(x))
}
object Identity {
	def apply(taxons_ : Taxon*) : Identity = new Identity(taxons_.toList)
	def apply(taxons_ : List[Taxon]) : Identity = new Identity(taxons_)

	implicit def fromTaxon(taxon : Taxon) : Identity = new Identity(taxon)
}

trait HasIdentity {
	val identity : Identity

	def isA(x : Taxon) = identity.isA(x)
}