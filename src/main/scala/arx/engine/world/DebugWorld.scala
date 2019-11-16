package arx.engine.world

import arx.application.Noto
import arx.core.introspection.{Clazz, Field, ReflectionAssistant}
import arx.core.traits.ArxNumeric
import arx.engine.data.{Moddable, Reduceable, TAuxData}
import arx.engine.entity.Entity
import arx.engine.event.GameEvent
import arx.engine.world.WorldQueryParser.parse
import fastparse.parse

import scala.annotation.tailrec
import scala.reflect.ClassTag

object DebugWorld {
	def selectFields(field : Field[_ <: AnyRef,_ <: AnyRef]*) = new WorldQuery().selectFields(field:_*).materialize(world.view)
	def selectData(clazz : Clazz[_ <: AnyRef]*) = new WorldQuery().selectData(clazz:_*).materialize(world.view)
	def selectEvents() = new WorldQuery().selectEvents().materialize(world.view)

	def query(str : String) : WorldQuery = {
//		WorldQueryParser.parse(str)
		???
	}

	var world : World = _
	var displayWorld : World = _
}


class WorldQuery {
	var selections : Vector[(String, (WorldView, Entity) => Option[Any])] = Vector()
	var selectedData : Vector[Clazz[_]] = Vector()
	var shouldSelectEvents : Boolean = false
//	var dataConditions : Vector[(WorldView, Entity) => Boolean] = Vector()
	var entityConditions : Vector[(WorldView, Entity) => Boolean] = Vector()
	var entityAssertions : Vector[(String, (WorldView, Entity) => Boolean, (WorldView, Entity) => Map[String, Any])] = Vector()
	var eventConditions : Vector[(WorldView, GameEvent) => Boolean] = Vector()

	def selectFields(field : Field[_,_ ]*) : this.type = {
		selectFieldsSeq(field.toVector)
	}
	def selectFieldsSeq(fields : Seq[Field[_,_]]) : this.type = {
		val res = fields.map(f => {
			val extractor = (view : WorldView, entity: Entity) => {
				view.dataOptByClass(entity, f.clazz.runtimeClass)
					.map(d => f.getter.asInstanceOf[Any => Any].apply(d))
			}
			f.name -> extractor
		})
		selections ++= res
		this
	}
	def selectData(clazz : Clazz[_]*) = { selectedData ++= clazz.toVector; this }
	def selectEvents() = { shouldSelectEvents = true; this }

//	def where[C <: TAuxData] (condition : C => Boolean)(implicit classTag : ClassTag[C]) = {
//		dataConditions :+= ((v : WorldView, e : Entity) => v.dataOpt[C](e).exists(d => condition(d)))
//		this
//	}
//	def where[C <: TAuxData](clazz : Clazz[C], condition : C => Boolean) = {
//		dataConditions :+= ((v : WorldView, e : Entity) => v.dataOptByClass[C](e, clazz.runtimeClass).exists(d => condition(d)))
//		this
//	}

	def whereId(id : Long) : WorldQuery = whereEntity(new Entity(id))
	def whereEntity(target : Entity) : WorldQuery  = {
		entityConditions :+= ((v : WorldView, e : Entity) => e == target)
		this
	}

	def whereEventOfType(eventType : Class[_]) = {
		eventConditions :+= ((v : WorldView, e : GameEvent) => e.getClass == eventType)
		this
	}
	def whereEvent[C](eventType : Class[C], condition : C => Boolean) = {
		eventConditions :+= ((v : WorldView, e : GameEvent) => {
			if (e.getClass == eventType) {
				condition(e.asInstanceOf[C])
			} else { false }
		})
		this
	}

	def whereEntityHasData(clazz : Clazz[_]) = {
		entityConditions :+= ((v : WorldView, e : Entity) => v.hasDataByClass(e, clazz.runtimeClass))
		this
	}

	def materialize(view : WorldView) : MaterializedWorldQuery = new MaterializedWorldQuery(this, view)

	def run(view : WorldView) : WorldQueryResult = {

		var entityResults = Map[Entity,Map[String,Option[Any]]]()
		var assertionResults = Map[Entity,(Map[String,Boolean], Map[String,Map[String,Any]])]()
		if (!shouldSelectEvents) {
			val matchedEntities = view.entities.filter(e => entityConditions.forall(cond => cond(view,e)))
   				.filter(e => selectedData.exists(d => view.hasDataByClass(e, d.runtimeClass)))

			entityResults = matchedEntities.map(e => {
				val fieldResults = selections.map(f => {
					val (key, extractor) = f
					val extractedValue = extractor(view, e)
					key -> extractedValue
				}).toMap

				e -> fieldResults
			}).toMap

			assertionResults = matchedEntities.map(e => {
				val subCorrectnessResults = entityAssertions.map {
					case (key, assrt, _) => key -> assrt(view, e)
				}.toMap

				val subInfoResults = entityAssertions.map {
					case (key, _, subPiecesFunc) => key -> subPiecesFunc(view, e)
				}.toMap

				e -> (subCorrectnessResults, subInfoResults)
			}).toMap
		}

		val eventResults = if (!shouldSelectEvents) { Vector() }
		else {
			view.events.filter(e => eventConditions.forall(condition => condition(view, e)))
		}


		WorldQueryResult(entityResults, eventResults, assertionResults)
	}
}

object WorldQuery {
	def run(str : String)(implicit world : WorldView) : WorldQueryResult = WorldQueryParser.parse(str, Some(world)).map(_.run(world)).getOrElse(WorldQueryResult.error(s"Invalid query $str"))
	def assert(str : String, message : String = "")(implicit world : WorldView) : Unit = {
		val queryStr = if (!str.toLowerCase().startsWith("assert ")) {"assert " + str} else { str }
		WorldQueryParser.parse(queryStr, Some(world)) match {
			case Some(q) =>
				val res = q.run(world)
				if (!res.assertionsPassed) {
					Noto.error("Assertions failed")
					res.assertionResults.filter(e => e._2._1.exists(!_._2))
						.foreach {
							case (entity, (correctnessResults, infoResults)) =>
								Noto.info(s"$entity :")
								correctnessResults.filter(!_._2).keys.foreach(s => {
									Noto.info(s"\t$s : failed")
									Noto.info("\tActual values:")
									infoResults.getOrElse(s,Map()).foreach {
										case (name, value) => Noto.info(s"\t\t$name: $value")
									}
								})
						}
					var failureStr = "Query assertions failed"
					if (message.nonEmpty) {
						failureStr += s"\n\tmessage: $message"
					}
					throw new AssertionError(failureStr + s"\n\tquery: $queryStr")
				} else if (res.assertionResults.isEmpty) {
					Noto.error("Assert query yielded no results")
					if (message.nonEmpty) { Noto.error(s"\t$message") }
					throw new AssertionError()
				}
			case None => throw new AssertionError(s"invalid query for assertion $queryStr")
		}
	}
}


class MaterializedWorldQuery (query : WorldQuery, view : WorldView) {
	def run = query.run(view)
}

case class WorldQueryResult(
										entityResults : Map[Entity, Map[String, Option[Any]]],
										eventResults : Iterable[GameEvent],
										assertionResults : Map[Entity, (Map[String, Boolean], Map[String, Map[String, Any]])],
										errorMessage : Option[String] = None) {
	override def toString: String = {
		val stringBuilder = new StringBuilder
		entityResults.foreach {
			case (entity, results) => {
				if (stringBuilder.nonEmpty && stringBuilder.last != '\n') {stringBuilder.append("\n")}
				stringBuilder.append(s"$entity:\n")
				results.toList.sortBy(_._1).foreach {
					case (identifier, result) => {
						if (stringBuilder.nonEmpty && stringBuilder.last != '\n') {stringBuilder.append("\n")}
						for (resultStr <- result.map(r => r.toString)) {
							stringBuilder.append(s"\t$identifier :")
							val adjustedResultStr = if (resultStr.contains('\n')) {
								"\n\t\t" + resultStr.replaceAll("\n", "\n\t\t")
							} else {
								" " + resultStr
							}

							stringBuilder.append(adjustedResultStr)
						}
					}
				}
			}
		}

		eventResults.foreach(e => {
			stringBuilder.append("- ").append(e.toString).append("\n")
		})

		stringBuilder.mkString
	}

	def assertionsPassed : Boolean = assertionResults.forall(_._2._1.forall(_._2))
}
object WorldQueryResult {
	def error(str : String) = WorldQueryResult(Map(), Nil, Map(), Some(str))
}

object WorldQueryParser {

//
//	def parse(str : String) : WorldQuery = {
//
//	}

	@tailrec
	def toDouble(a : Any) : Option[Double] = a match {
		case Some(w) => toDouble(w)
		case r : Reduceable[_] => toDouble(r.currentValue)
		case b : Byte => Some(b.toDouble)
		case s : Short => Some(s.toDouble)
		case i : Int => Some(i.toDouble)
		case l : Long => Some(l.toLong)
		case f : Float => Some(f.toDouble)
		case d : Double => Some(d)
		case _ => None
	}

	val allClasses = ReflectionAssistant.instancesOfSubtypesOf[Clazz[_]]
	val classesByName : Map[String,Clazz[_]] = allClasses.map(c => c.className -> c).toMap
	def lookupClassByName(str : String) : Option[Clazz[_]] = {
		val ret = classesByName.get(str)
		if (ret.isEmpty) {
			Noto.warn(s"No class named $str found, could not resolve FROM clause")
		}
		ret
	}

	trait Comparison {
		def compare(a : Any, b : Any) : Boolean

		override def toString: String = comparisonToString.getOrElse(this, super.toString)
	}
	trait NumericComparison extends Comparison {

		override def compare(ar: Any, br: Any): Boolean = {
			(toDouble(ar), toDouble(br)) match {
				case (Some(a),Some(b)) => expectedCompareToValues.contains(a.compareTo(b))
				case _ => false
			}
		}
		def expectedCompareToValues : Set[Int]
	}

	@scala.annotation.tailrec
	def resolveObject(o : Any) : Any = o match {
		case r : Reduceable[_] => r.currentValue
		case Some(s) => resolveObject(s)
		case o => o
	}

	case object LT extends NumericComparison { override def expectedCompareToValues: Set[Int] = Set(-1) }
	case object GT extends NumericComparison { override def expectedCompareToValues: Set[Int] = Set(1) }
	case object LEQ extends NumericComparison { override def expectedCompareToValues: Set[Int] = Set(-1, 0) }
	case object GEQ extends NumericComparison { override def expectedCompareToValues: Set[Int] = Set(0, 0) }
	case object EQ extends Comparison { override def compare(a: Any, b: Any): Boolean = resolveObject(a) == resolveObject(b) }
	case object NEQ extends Comparison { override def compare(a: Any, b: Any): Boolean = resolveObject(a) != resolveObject(b) }

	trait BooleanComparison extends Comparison {
		@tailrec
		final def toBoolean(a : Any) : Option[Boolean] = a match {
			case Some(b) => toBoolean(b)
			case b : Boolean => Some(b)
			case _ => None
		}
		override def compare(ar: Any, br: Any): Boolean = {
			(toBoolean(ar), toBoolean(br)) match {
				case (Some(a),Some(b)) => compareBooleans(a,b)
				case _ => false
			}
		}
		def compareBooleans(a: Boolean, b: Boolean) : Boolean
	}
	case object AND extends BooleanComparison { override def compareBooleans(a : Boolean, b: Boolean) : Boolean = a && b }
	case object OR extends BooleanComparison {
		override def compareBooleans(a : Boolean, b: Boolean) : Boolean = a || b
	}
	val comparisonsByString = Map[String, Comparison]("<" -> LT, ">" -> GT, "<=" -> LEQ, ">=" -> GEQ, "==" -> EQ, "!=" -> NEQ, "and" -> AND, "or" -> OR)
	val comparisonToString = comparisonsByString.map(t => t._2 -> t._1)



	sealed trait Expression {
		def name : String
		override def toString: String = name
		def constant : Boolean = false
	}
	case class Constant(value : Any) extends Expression {
		def name = value.toString
		override def constant = true
	}
	case class Glob(context : Option[String] = None) extends Expression {
		override def name: String = "*"
	}
	case class UntypedReference(key : String) extends Expression {
		def name = key
		def resolve(classes : Seq[Clazz[_]]): Expression = {
			var found : Vector[Field[_,_]] = Vector()
			classes.foreach(cz => {
				cz.fields.get(key) match {
					case Some(f) => found :+= f
					case None => // do nothing
				}
			})
//			classes.flatMap(cz => cz.fields.get(key))
			if (found.isEmpty) {
				Noto.error("Could not resolve select for \"" + key + "\"")
				return Constant(None)
			}
			if (found.size > 1) {
				Noto.error("\"" + key + "\" resolved to multiple fields")
			}

			Reference(found.head)
		}
	}
	case class Reference(field : Field[_,_]) extends Expression {
		def name = field.clazz.className + "." + field.name
	}
	case object EntityId extends Expression {
		override def name: String = "id"
	}

	case class Predicate(left : Expression, comparison : Comparison, right : Expression) extends Expression {
		override def name: String = s"${left.name} $comparison ${right.name}"
	}

	trait Clause
	case class FromClause(classes : Seq[Clazz[_]]) extends Clause
	case class SelectClause(fields : Seq[Expression]) extends Clause {}
	case class WhereClause(condition : Predicate) extends Clause
	case class AssertClause(condition : Predicate) extends Clause


	case class ParsedQuery(clauses : Seq[Clause])

	import fastparse._, NoWhitespace._

	protected def ws[_ : P] = " ".rep
	protected def commaSeparator[_ : P] = "," ~ ws.?
	protected def boolOp[_ : P] = P(IgnoreCase("and") | IgnoreCase("or")).!.map(s => comparisonsByString(s.toLowerCase()))
	protected def comparator[_ : P] = P("<" | ">" | "<=" | ">=" | "==" | "!=").!.map(s => comparisonsByString(s))
	protected def digits[_: P] = P( CharsWhileIn("0-9") )
	protected def fractional[_: P] = P( "." ~ digits )
	protected def integral[_: P] = P( "0" | CharIn("1-9")  ~ digits.? )
	protected def entityId[_: P] = P(IgnoreCase("Entity(") ~ CharsWhileIn("0-9").! ~ ")").map(s => Constant(s.toInt))
	protected def number[_ : P] = P(CharIn("+\\-").? ~ integral ~ fractional.?).!.map(x => Constant(x.toDouble))
	protected def none[_ : P] = P(IgnoreCase("none") | IgnoreCase("null")).map(_ => Constant(None))
	protected def term[_ : P] = P(CharPred(c => c.isLetter) ~ CharPred(c => c.isLetterOrDigit).rep).!.map(x => UntypedReference(x))
	protected def glob[_ : P] = P((term ~ ".").?.! ~ "*").map(ref => Glob(Option(ref).filter(r => r.nonEmpty).map(_.replace(".",""))))
	protected def id[_ : P] = P(IgnoreCase("id")).map(_ => EntityId)
	protected def value[_ : P] = P(none | id | entityId | term | number)
	protected def expression[_ : P] : P[Expression] = P(value | predicate)
	protected def basePredicate[_ : P] : P[Predicate] = P(expression ~ ws.? ~ comparator ~ ws.? ~ expression).map{ case (l,c,r) => Predicate(l,c,r) }
	protected def compoundPredicate[_ : P] : P[Predicate] = P(basePredicate ~ ws ~ boolOp ~ ws ~ predicate).map{ case (l,c,r) => Predicate(l,c,r) }
	protected def predicate[_ : P] = P(compoundPredicate | basePredicate)
	protected def selectTarget[_ : P] = P(glob | expression)
	protected def select[_ : P] = P(IgnoreCase("select") ~ ws ~ selectTarget.rep(sep = commaSeparator./)).map(strs => SelectClause(strs))
	protected def from[_ : P] = P(IgnoreCase("from") ~ ws ~ term.rep(sep = commaSeparator./)).map(strs => FromClause(strs.flatMap(s => lookupClassByName(s.key))))
	protected def assert[_ : P] = P(IgnoreCase("assert") ~ ws ~ predicate).map(c => AssertClause(c))

	protected def subject[_ : P] = predicate | expression

	protected def where[_ : P] = P(IgnoreCase("where") ~ ws ~ predicate).map(c => WhereClause(c))
	protected def clause[_ : P] = P(select | from | where | assert)
	protected[world] def parseExpression[_: P] = P(clause.rep(sep = ws./) ~ End).map(s => ParsedQuery(s))


	def parse(str : String, view : Option[WorldView] = None) : Option[WorldQuery] = {
		val raw = fastparse.parse(str, parseExpression(_))
		raw match {
			case Parsed.Success(q, successIndex) =>
				Noto.debug(s"Query parsed successfully: $q")
				val wq = new WorldQuery()

				val fromClauseClasses = q.clauses.collect { case from : FromClause => from }.flatMap(f => f.classes)
				implicit val selectedClasses = (if (fromClauseClasses.isEmpty) {
					view.map(v => v.dataStores.keys.flatMap(cz => allClasses.find(clazz => clazz.runtimeClass == cz))).getOrElse(Nil)
				} else {
					fromClauseClasses
				}).toVector

				wq.selectedData = selectedClasses.distinct
				q.clauses.foreach {
					case SelectClause(selections) =>
						wq.selections ++= selections.flatMap(s => expandGlobs(s)).map(v => v.name -> expressionToSelection(v))
					case WhereClause(condition) =>
						wq.entityConditions :+= conditionToEntityCondition(condition)
					case AssertClause(condition) =>
						wq.entityAssertions :+= (condition.toString, conditionToEntityCondition(condition), expressionToInfoSelection(condition))
					case _ => // do nothing
				}


				Some(wq)
			case f @ Parsed.Failure(failureString, index, extra) =>
				Noto.error(f.trace(true).toString)
				None
		}
	}

	protected def expandGlobs(selection : Expression)(implicit selectedClasses : Seq[Clazz[_]]) : Seq[Expression] = {
		selection match {
			case Glob(contextOpt) => contextOpt match {
				case Some(context) =>selectedClasses.filter(c => c.className == context).flatMap(c => c.allFields.map(f => Reference(f)))
				case None => selectedClasses.flatMap(c => c.allFields.map(f => Reference(f)))
			}
			case other => Seq(other)
		}
	}

	protected def conditionToEntityCondition(condition : Predicate)(implicit selectedClasses : Seq[Clazz[_]]) : (WorldView,Entity) => Boolean = {
		val expr = expressionToSelection(condition)
		(view, entity) => {
			expr(view, entity) match {
				case Some(v) => v match {
					case b: Boolean => b
					case _ =>
						Noto.error(s"Non boolean evaluation result from condition : $condition")
						false
				}
				case None => false
			}
		}
	}

	protected def expressionToInfoSelection(value : Expression)(implicit selectedClasses : Seq[Clazz[_]]) : (WorldView, Entity) => Map[String, Any] = value match {
		case Constant(_) => (_,_) => Map()
		case utr @ UntypedReference(_) => expressionToInfoSelection(utr.resolve(selectedClasses))
		case Reference(field) => (view, entity) => {
			val dopt = view.dataOptByClass(entity, field.clazz.runtimeClass)
			dopt.map(d => Map(value.name -> field.getter.asInstanceOf[Any => Any].apply(d))).getOrElse(Map())
		}
		case Predicate(left, _, right) =>
			val leftInfo = expressionToInfoSelection(left)
			val rightInfo = expressionToInfoSelection(right)
			(view, entity) =>  leftInfo(view, entity) ++ rightInfo(view, entity)
		case EntityId => (view, entity) => Map("id" -> entity.id)
	}

	protected def expressionToSelection(value : Expression)(implicit selectedClasses : Seq[Clazz[_]]) : (WorldView, Entity) => Option[Any] = value match {
		case Constant(any) =>
			if (any == None) {
				(_, _) => None
			} else {
				(_, _) => Some(any)
			}
		case utr @ UntypedReference(_) => expressionToSelection(utr.resolve(selectedClasses))
		case Reference(field) => (view, entity) => {
			val dopt = view.dataOptByClass(entity, field.clazz.runtimeClass)
			dopt.map(d => field.getter.asInstanceOf[Any => Any].apply(d))
		}
		case Predicate(left, comparison, right) =>
			val selectLeft = expressionToSelection(left)
			val selectRight = expressionToSelection(right)
			(view, entity) => {
				val left = selectLeft(view, entity)
				val right = selectRight(view, entity)
				Some(comparison.compare(left, right))
			}
		case EntityId => (view, entity) => Some(entity.id)
	}

}
