package arx.core.introspection

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/27/18
 * Time: 6:51 AM
 */

import java.nio.file.{Files, Path, Paths}

import arx.Prelude._
import arx.core.async.KillableThread.ApplicationLevel
import arx.core.async.{Executor, KillableThread, OnExitRegistry}
import arx.core.macros.GenerateCompanion
import arx.core.vec._
import arx.engine.data.{TAuxData, TNestedData, TTestAuxData}


abstract class Clazz[C](val className: String, val runtimeClass: Class[C]) {
	def Sentinel : C
	def instantiate : C = ReflectionAssistant.instantiate(runtimeClass)

	var fields: Map[String, Field[C, _]] = Map()

	def allFields = fields.values

	override def toString: String = className

	def copyInto(from : C, to : C)
	def copyIntoUntyped(from : AnyRef, to : AnyRef): Unit = {
		copyInto(from.asInstanceOf[C], to.asInstanceOf[C])
	}
}

object Clazz {
	lazy val allClazzes : List[_ <: Clazz[_]] = ReflectionAssistant.instancesOfSubtypesOf(classOf[Clazz[_]])
	lazy val fromName : Map[String, Clazz[_]] = allClazzes.map(c => c.className -> c).toMap
	private lazy val _fromClass : Map[Class[_], Clazz[_]] = allClazzes.map(c => c.runtimeClass -> c).toMap

	def fromClass[T](klass : Class[T]) : Clazz[T] = _fromClass(klass).asInstanceOf[Clazz[T]]
	def fromClassOpt[T](klass : Class[T]) : Option[Clazz[T]] = _fromClass.get(klass).asInstanceOf[Option[Clazz[T]]]

}

trait UntypedField {
	def getValue(objectReference: AnyRef): Any

	def setValue(objectReference: AnyRef, value: Any)

	def name: String
}

case class Field[C, T](name: String, getter: (C) => T, setter: (C, T) => Unit, clazz: Clazz[C]) extends UntypedField {

	override def getValue(objectReference: AnyRef): Any = {
		objectReference match {
			case ref: C => getter(ref)
			case other => throw new IllegalStateException("Untyped getValue(...) on field violated type constraints at runtime")
		}
	}

	override def setValue(objectReference: AnyRef, value: Any): Unit = {
		objectReference match {
			case ref: C => {
				value match {
					case validValue: T => setter(ref, validValue)
					case other => throw new IllegalStateException("Untyped setValue(...) on field violated type constraints of value at runtime")
				}
			}
			case other => throw new IllegalStateException("Untyped setValue(...) on field violated type constraints at runtime")
		}
	}
}

//class FieldBuilder[C](name : String) {
//	def withAccessors[T](getter : (C) => T, setter : (C,T) => Unit) : Field[C,T] = Field(name,getter,setter)
//}
//
//object Field {
//	def createFromExample[C,T](name : String, getter : (C) => T, setter : (C,T) => Unit, classValue : C, value : T) : Field[C,T] = {
//		Field(name,getter,setter)
//	}
////	def apply[C](name : String) : FieldBuilder[C] = new FieldBuilder[C](name)
//}

/**
 * Workaround for the limitations of reflection and type inference. Because the actual generic type is not known at
 * runtime we can't rely on it when generating the code, or we just end up with Option[_] instead of Option[String].
 * But, if we create a sentinel instance in the companion object, and make use of references to the concrete values
 * it contains, we can convince scala's type inference to figure out which type things should be
 */
class FieldExample[T](value: T) {
	def createField[C](name: String, getter: (C) => T, setter: (C, T) => Unit, clazz: Clazz[C]): Field[C, T] = {
		Field[C, T](name, getter, setter, clazz)
	}
}

object Field {
	def fromValue[T](value: T): FieldExample[T] = new FieldExample(value)
}


object FieldGenerator {
	def main(args: Array[String]): Unit = {
		ReflectionAssistant.ignoreEagerSingletons.set(true)
		generate(args.headOption.getOrElse(""), testClasses = if (args.length == 2) { args(1).toBoolean } else { false })
		KillableThread.kill(ApplicationLevel)
		Executor.onQuit()
		OnExitRegistry.onExit()
	}

	def generate(prefixFilter : String, testClasses : Boolean) {
		for ((packageName, clazzes) <- (ReflectionAssistant.allSubTypesOf[TAuxData] ::: ReflectionAssistant.allSubTypesOf[TNestedData])
		   .filter(clazz => clazz.getPackage.getName.startsWith(prefixFilter))
		   .filter(clazz => classOf[TTestAuxData].isAssignableFrom(clazz) == testClasses)
			.filter(clazz => clazz.getAnnotations.exists(a => a.annotationType() == classOf[GenerateCompanion]))
			.groupBy(st => st.getPackage.getName)) {
			val body = new StringBuilder

			def addLine(s: String): Unit = {
				body.append(s).append('\n')
			}

			addLine(s"package $packageName")
			addLine("import arx.core.introspection.Field")
			addLine("import arx.core.introspection.Clazz")

			addLine("object Companions {")

			for (clazz <- clazzes) {
				addLine(s"import ${clazz.getCanonicalName}")
				val className = clazz.getSimpleName
				addLine(s"""object ${className} extends Clazz[$className]("$className", classOf[$className]){""")
				addLine(s"\tval Sentinel = new $className")
				addLine(s"\toverride def instantiate = new $className")

				val fieldNames = clazz.getDeclaredFields.map(f => {
					val name = f.getName
					if (name.startsWith("_")) {
						name.substring(1)
					} else {
						name
					}
				})

				for (fieldName <- fieldNames) {
					addLine(s"""\tval $fieldName = Field.fromValue(Sentinel.$fieldName).createField[$className]("$fieldName",f => f.$fieldName, (f,$fieldName) => f.$fieldName = $fieldName, $className) """)
					addLine(s"""\tfields += "$fieldName" -> $fieldName""")
				}
				addLine(
					s"""
						|\tdef apply(f : $className => Unit) : $className = { val v = new $className; f(v); v }
					 """.stripMargin)

				addLine(s"\tdef copyInto(from : $className, to : $className) {")
				for (fieldName <- fieldNames) {
					addLine(s"\t\tto.$fieldName = from.$fieldName")
				}
				addLine(s"\t}")



				addLine("}")
			}

			addLine("}")

//			val genSrcDir = if (testClasses) { "generated-test-sources" } else { "generated-sources" }
			val genSrcDir = if (testClasses) { "test" } else { "main" }
//			val dirName = s"target/$genSrcDir/scala/${packageName.replace('.', '/')}"
			val dirName = s"src/$genSrcDir/scala/${packageName.replace('.', '/')}"
			val fileName =  dirName + "/Companions.scala"
			Files.createDirectories(Paths.get(dirName))
			writeToFile(fileName, body.toString())
		}


		Executor.onQuit()
	}
}