package arx.engine.data

import java.lang.reflect.ParameterizedType

import arx.application.{Noto, TLoggingLevelProvider}
import arx.core.introspection.ReflectionAssistant
import arx.core.math.Sext
import arx.core.representation.{ConfigValue, StringConfigValue}
import arx.core.vec.{ReadVec2f, ReadVec2i, ReadVec3f, ReadVec3i, ReadVec4f, ReadVec4i}
import arx.graphics.helpers.{Color, RGBA}
import overlock.atomicmap.AtomicMap

import scala.reflect.runtime.{universe => u}
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object ConfigDataLoader {

	var operationsByType = AtomicMap.atomicNBHM[Class[_], List[(_, ConfigValue) => Unit]]

	var supportedSimpleTypes = List(classOf[String], classOf[Integer], classOf[Float], classOf[Double], classOf[Boolean])

	private def extractConfigValueFunctionForType(t: Type): Option[ConfigValue => Any] = {
		Option(if (t == typeOf[Int]) {
			c => c.int
		} else if (t == typeOf[Float]) {
			c => c.float
		} else if (t == typeOf[Double]) {
			c => c.float
		} else if (t == typeOf[String]) {
			c => c.str
		} else if (t == typeOf[Boolean]) {
			c => c.bool
		} else if (t == typeOf[ReadVec4f]) {
			c => c.v4
		} else if (t == typeOf[ReadVec3f]) {
			c => c.v3
		} else if (t == typeOf[ReadVec2f]) {
			c => c.v2
		} else if (t == typeOf[ReadVec4i]) {
			c => ReadVec4i(c.v4)
		} else if (t == typeOf[ReadVec3i]) {
			c => ReadVec3i(c.v3)
		} else if (t == typeOf[ReadVec2i]) {
			c => ReadVec2i(c.v2)
		} else if (t == typeOf[Color]) {
			c => RGBA(c.v4)
		} else if (t == typeOf[Sext]) {
			c => Sext.closestTo(c.float)
		} else {
			null
		})
	}

	private def computeOperationsForType[T](implicit classTag: ClassTag[T]): List[(AnyRef, ConfigValue) => Unit] = {
		val sType = ReflectionAssistant.toScalaType(classTag.runtimeClass)
		computeOperationsForType(sType)
	}

	private def computeOperationsForType(sType: ClassSymbol): List[(AnyRef, ConfigValue) => Unit] = {
		val operations: List[(AnyRef, ConfigValue) => Unit] = sType.selfType.members.collect {
			case setter: MethodSymbol if setter.isSetter =>
				val getter = setter.getter.asInstanceOf[MethodSymbol]
				val fieldName = getter.name.toString

				extractConfigValueFunctionForType(getter.returnType) match {
					case Some(extractor) => (data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(extractor(c)))
					case None => {
						if (getter.returnType.typeSymbol == typeOf[Moddable[Any]].typeSymbol) {
							val moddableOf = getter.returnType.typeArgs.head
							extractConfigValueFunctionForType(moddableOf) match {
								case Some(extractor) => (data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(Moddable(extractor(c))))
								case None => null
							}
						} else if (getter.returnType.typeSymbol == typeOf[List[Any]].typeSymbol) {
							val listType = getter.returnType.typeArgs.head
							extractConfigValueFunctionForType(listType) match {
								case Some(f) =>
									(data: AnyRef, config: ConfigValue) => {
										val subConf = config.field(fieldName)
										if (subConf.isArr) {
											var res = ReflectionAssistant.invoke(data, getter)().asInstanceOf[List[Any]].take(0)
											subConf.arr.foreach(v => {
												res ::= f(v)
											})
											ReflectionAssistant.invoke(data, setter)(res.reverse)
										}
									}: Unit
								case None =>
									Noto.fine(ConfigLoadLogging, s"Could not find extraction functions for values of list ${getter.returnType}")
									null
							}
						} else if (getter.returnType.typeSymbol == typeOf[Map[Any, Any]].typeSymbol) {
							val keyType = getter.returnType.typeArgs(0)
							val valueType = getter.returnType.typeArgs(1)

							(extractConfigValueFunctionForType(keyType), extractConfigValueFunctionForType(valueType)) match {
								case (Some(keyFunc), Some(valueFunc)) =>
									(data: AnyRef, config: ConfigValue) => {
										val subConf = config.field(fieldName)
										if (subConf.isObj) {
											var res = ReflectionAssistant.invoke(data, getter)().asInstanceOf[Map[Any, Any]].take(0)
											for ((k,v) <- subConf.fields) {
												val key = keyFunc(new StringConfigValue(k))
												val value = valueFunc(v)
												res += key -> value
											}

											ReflectionAssistant.invoke(data, setter)(res)
										}
									}: Unit
								case _ =>
									Noto.fine(ConfigLoadLogging, s"Could not find extraction functions for key/value pairs of map ${getter.returnType}")
									null
							}
						} else if (getter.returnType.typeSymbol == typeOf[Reduceable[Any]].typeSymbol) {
							val reduceableType = getter.returnType.typeArgs.head
							extractConfigValueFunctionForType(reduceableType) match {
								case Some(f) =>
									(data: AnyRef, config: ConfigValue) => {
										val subField = config.field(fieldName)
										if (subField.nonEmpty) {
											val base = ReflectionAssistant.invoke(data, getter)().asInstanceOf[Reduceable[Any]]
											ReflectionAssistant.invoke(data, setter)(base.withBaseValue(f(subField)))
										}
									}: Unit
								case None => null
							}
						} else if (getter.returnType.typeSymbol == typeOf[Option[Any]].typeSymbol) {
							val optionType = getter.returnType.typeArgs.head
							extractConfigValueFunctionForType(optionType) match {
								case Some(f) =>
									(data: AnyRef, config: ConfigValue) => {
										val subField = config.field(fieldName)
										if (subField.nonEmpty) {
											val substr = subField.str
											if (substr == "none" || substr == "None") {
												ReflectionAssistant.invoke(data, setter)(None)
											} else {
												ReflectionAssistant.invoke(data, setter)(Some(f(subField)))
											}
										}
									}: Unit
								case None => null
							}
						} else if (getter.returnType <:< typeOf[ConfigLoadable]) {
							(data : AnyRef, config : ConfigValue) => {
								val subField = config.field(fieldName)
								if (subField.nonEmpty) {
									val curValue = ReflectionAssistant.invoke(data, getter)().asInstanceOf[ConfigLoadable]
									curValue.loadFromConfig(subField)
								}
							} : Unit
						} else if (getter.returnType.typeSymbol.isClass && getter.returnType.typeSymbol.asClass.isCaseClass) {
							val subTypeOperations = computeOperationsForType(getter.returnType.typeSymbol.asClass)
							(data: AnyRef, config: ConfigValue) => {
								val subConf = config.field(fieldName)
								val subObj = ReflectionAssistant.invoke(data, getter)().asInstanceOf[AnyRef]
								if (subConf.isObj) {
									subTypeOperations.foreach(op => op(subObj, subConf))
								}
							}
						} else {
							null
						}
					}
				}
			//				if (getter.returnType == typeOf[String]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(c.str))
			//				} else if (getter.returnType == typeOf[Int]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(c.int))
			//				} else if (getter.returnType == typeOf[Float]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(c.float))
			//				} else if (getter.returnType == typeOf[Boolean]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(c.bool))
			//				} else if (getter.returnType == typeOf[ReadVec4f]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(c.v4))
			//				} else if (getter.returnType == typeOf[Moddable[String]]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(Moddable(c.str)))
			//				} else if (getter.returnType == typeOf[Moddable[Int]]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(Moddable(c.int)))
			//				} else if (getter.returnType == typeOf[Moddable[Float]]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(Moddable(c.float)))
			//				} else if (getter.returnType == typeOf[Moddable[Boolean]]) {
			//					(data: AnyRef, config: ConfigValue) => config.field(fieldName).ifPresent(c => ReflectionAssistant.invoke(data, setter)(Moddable(c.bool)))
			//				} else if (getter.returnType.typeSymbol == typeOf[List[Any]].typeSymbol) {
			//					val listType = getter.returnType.typeArgs.head
			//					extractConfigValueFunctionForType(listType) match {
			//						case Some(f) =>
			//							(data: AnyRef, config: ConfigValue) => {
			//								val subConf = config.field(fieldName)
			//								if (subConf.isArr) {
			//									var res = ReflectionAssistant.invoke(data, getter)().asInstanceOf[List[Any]].take(0)
			//									subConf.arr.foreach(v => {
			//										res ::= f(v)
			//									})
			//									ReflectionAssistant.invoke(data, setter)(res.reverse)
			//								}
			//							}: Unit
			//						case None => null
			//					}
			//				} else if (getter.returnType.typeSymbol.isClass && getter.returnType.typeSymbol.asClass.isCaseClass) {
			//					val subTypeOperations = computeOperationsForType(getter.returnType.typeSymbol.asClass)
			//					(data: AnyRef, config: ConfigValue) => {
			//						val subConf = config.field(fieldName)
			//						val subObj = ReflectionAssistant.invoke(data, getter)().asInstanceOf[AnyRef]
			//						if (subConf.isObj) {
			//							subTypeOperations.foreach(op => op(subObj, subConf))
			//						}
			//					}
			//				} else {
			//					null
			//				}
		}.filterNot(_ == null).toList

		operations
	}

	/**
	 * Attempts to load all of the simple values from the given config into the given data object.
	 */
	def loadSimpleValuesFromConfig[T](aux: T, config: ConfigValue): Unit = {
		val ops = operationsByType.getOrElseUpdate(aux.getClass, computeOperationsForType(ReflectionAssistant.toScalaType(aux.getClass))).asInstanceOf[List[(T, ConfigValue) => Unit]]
		ops.foreach(op => op.apply(aux, config))
	}

}

trait ConfigLoadable {
	final def loadFromConfig(config : ConfigValue): this.type = {
		if (config.nonEmpty) {
			if (autoLoadSimpleValuesFromConfig) {
				ConfigDataLoader.loadSimpleValuesFromConfig(this, config)
			}
			customLoadFromConfig(config)
		}
		this
	}
	def customLoadFromConfig(config : ConfigValue) : Unit = {}
	def autoLoadSimpleValuesFromConfig : Boolean = true
}

object ConfigLoadLogging extends TLoggingLevelProvider {
	var loggingLevel : Int = Noto.Info
}

//object Tmp {
//
//	import scala.reflect.runtime.{universe => u}
//	import scala.reflect.runtime.universe._
//
//	val classloader = Thread.currentThread().getContextClassLoader
//	val mirror = u.runtimeMirror(classloader)
//
//	class FooAD extends TAuxData {
//		var x: List[Int] = List()
//		var _y: Float = 3
//
//		def y = _y
//
//		def y_=(i: Int) {
//			_y = i
//		}
//
//		var modB: Moddable[Boolean] = Moddable(false)
//	}
//
//	def func[T](t: T): Unit = {
//		val c = mirror.staticClass(t.getClass.getCanonicalName)
//		val getters = c.selfType.members.collect { case ms: MethodSymbol if ms.isGetter => ms }
//		val floatGetters = getters.filter(g => g.typeSignature.resultType == typeOf[Float])
//		val listGetters = getters.filter(g => g.typeSignature.resultType.typeSymbol == typeOf[List[_]].typeSymbol)
//		val booleanGetters = getters.filter(g => g.typeSignature.resultType == typeOf[Moddable[Boolean]])
//		println("foo")
//	}
//
//	def main(args: Array[String]): Unit = {
//		val tmp = classOf[FooAD]
//
//		val fooad = new FooAD
//		func(fooad)
//
//	}
//}