package arx.core.introspection

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/5/15
  * Time: 10:42 AM
  */

import java.lang.reflect
import java.lang.reflect.Modifier
import java.net.{URL, URLClassLoader}
import java.util.concurrent.atomic.AtomicBoolean

import arx.Prelude
import arx.application.Noto
import arx.core.async.Executor
import arx.core.metrics.Metrics
import arx.core.traits.TNonDiscoverable
import org.reflections.Reflections
import org.reflections.scanners.SubTypesScanner
import org.reflections.util.{ClasspathHelper, ConfigurationBuilder}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.reflect.ClassTag
import arx.Prelude.toRichTimer


object ReflectionAssistant {
	import scala.reflect.runtime.universe._
	import scala.reflect.runtime.{universe => u}
	val classloader = Thread.currentThread().getContextClassLoader
	var ignoreEagerSingletons = new AtomicBoolean(false)
	val mirrorFuture = Executor.submitAsync(() => {
		val urls = ClasspathHelper.forPackage("arx")
		val urlsArray = Array.ofDim[URL](urls.size())
		urls.toArray(urlsArray)
		val filteredClassloader = new URLClassLoader(urlsArray)
		val ret = Metrics.timer("reflection.universe-init").timeStmt(u.runtimeMirror(filteredClassloader))
		Metrics.checkpoint("scala reflect universe loaded")
		ret
	})
	def mirror = {
		val ret = mirrorFuture.get()
		ret
	}


	val reflectionsFuture = Executor.submitAsync(loadReflections _)


	def reflections = reflectionsFuture.get()

	def ensureReflectionsLoaded() : Unit = reflectionsFuture.get()

	def loadReflections = {
		Metrics.timer("reflection-init").timeStmt {
			val ref = new Reflections(
				new ConfigurationBuilder()
					.setUrls(ClasspathHelper.forPackage("arx"))
					.setScanners(new SubTypesScanner()))

			Metrics.checkpoint("reflections read")
			import scala.collection.JavaConversions._
			if (!ignoreEagerSingletons.get()) {
				for (c <- ref.getSubTypesOf(classOf[TEagerSingleton])) {
					if (isSingleton(c)) {
						getSingleton(c)
					}
				}
			}
			Metrics.checkpoint("eager singletons initialized")

			ref
		}
	}

	//	val provideInstanceOfMem = memoize((clazz : Class[_]) => {
	//		provideInstanceOfIntern(clazz).asInstanceOf[Class[_]]
	//	})
	//
	//	def provideInstanceOf[T: Manifest] = provideInstanceOfMem[T].apply(manifest[T].runtimeClass)
	//
	//	def provideInstanceOf[T](clazz: Class[T]) = provideInstanceOfMem[T].apply(clazz)
	//
	//	protected def provideInstanceOfIntern[T](clazz: Class[_]) = {
	//		val subClasses = reflections.getSubTypesOf[T](clazz.asInstanceOf[Class[T]])
	//		if (!clazz.isInterface && !Modifier.isAbstract(clazz.getModifiers)) {
	//			subClasses.add(clazz.asInstanceOf[Class[T]])
	//		}
	//
	//		if (subClasses.isEmpty) {
	//			throw new IllegalArgumentException("No implementation available for class: " + clazz.getName)
	//		}
	//
	//		subClasses.iterator().next().newInstance().
	//		// sort by some means
	//	}
	//
	def arrayOf[T](c: Class[T], size: Int) = {
		if (c == classOf[Integer]) {
			java.lang.reflect.Array.newInstance(classOf[Int], size).asInstanceOf[Array[T]]
		} else if (c == classOf[java.lang.Float]) {
			java.lang.reflect.Array.newInstance(classOf[Float], size).asInstanceOf[Array[T]]
		} else if (c == classOf[java.lang.Double]) {
			java.lang.reflect.Array.newInstance(classOf[Double], size).asInstanceOf[Array[T]]
		} else if (c == classOf[java.lang.Short]) {
			java.lang.reflect.Array.newInstance(classOf[Short], size).asInstanceOf[Array[T]]
		} else if (c == classOf[java.lang.Byte]) {
			java.lang.reflect.Array.newInstance(classOf[Byte], size).asInstanceOf[Array[T]]
		} else if (c == classOf[java.lang.Long]) {
			java.lang.reflect.Array.newInstance(classOf[Long], size).asInstanceOf[Array[T]]
		} else {
			java.lang.reflect.Array.newInstance(c, size).asInstanceOf[Array[T]]
		}
	}

	def allSubTypesOf(clazz: Class[_]): List[Class[_]] = reflections.getSubTypesOf(clazz).toList.filterNot(cz => classOf[TNonDiscoverable].isAssignableFrom(cz))
	def allSubTypesOf[T: Manifest]: List[Class[_ <: T]] = allSubTypesOf(manifest[T].runtimeClass).asInstanceOf[List[Class[_ <: T]]]

	def isSingleton(c: Class[_]) = c.getSimpleName.endsWith("$") && !c.getSimpleName.startsWith("$") && c.getFields.exists(f => f.getName == "MODULE$")
	def getSingleton(c: Class[_]) = {
		try {
			var moduleField = c.getField("MODULE$").get(null)
			if (moduleField == null) {
				val constructor = c.getDeclaredConstructor()
				constructor.setAccessible(true)
				constructor.newInstance()

				Noto.warn("null valued MODULE$ encountered while loading a singleton. \n" +
					"Singleton : " + c.getSimpleName + "\n" +
					"It is likely this is a result of a static initializer loop stemming from serialization. \n" +
					"We are going to bypass this by creating a new instance, this will prevent catastrophic failure \n" +
					"but violates the singleton contract. Game archetypes that get loaded this way will not necessarily \n" +
					"be equal to other uses of themselves, for example. It is highly advised that you look for and \n" +
					"remove the source of any loops/circular references that might be causing this.")

				moduleField = c.getField("MODULE$").get(null)
			}
			moduleField
		} catch {
			case e : ExceptionInInitializerError =>
				Noto.error(s"Error initializing $c")
				throw e
		}
	}
	def instancesOf[T](l: List[Class[_]]): List[T] = {
		var ret = List[T]()
		for (clazz <- l if !classOf[TNonDiscoverable].isAssignableFrom(clazz)) {
			if (isSingleton(clazz)) {
				val singleton = getSingleton(clazz).asInstanceOf[T]
				ret ::= singleton
			} else {
				val constructorOpt = clazz.getConstructors find {_.getParameterTypes.length == 0}
				constructorOpt match {
					case Some(constructor) => try {
						ret ::= constructor.newInstance().asInstanceOf[T]
					} catch {
						case e: Exception =>
							e.printStackTrace
					}
					case None => Noto.finest("No default args constructor for class " + clazz.getName + ", cannot instantiate through reflection assistant")
				}
			}
		}
		ret
	}

	def firstInstanceOf[T](l: List[Class[_]]): Option[T] = {
		for (clazz <- l) {
			if (isSingleton(clazz)) {return Some(getSingleton(clazz).asInstanceOf[T])}
			else {
				val constructorOpt = clazz.getConstructors find {_.getParameterTypes.length == 0}
				constructorOpt match {
					case Some(constructor) =>
						try {
							return Some(constructor.newInstance().asInstanceOf[T])
						} catch {
							case e: Exception =>
								Noto.error("Error while calling constructor for class : " + clazz.getSimpleName + " : " + e); e.printStackTrace()
						}
					case None => Noto.finest("No default args constructor for class " + clazz.getName + ", cannot instantiate through reflection assistant")
				}
			}
		}
		None
	}

	def instancesOfSubtypesOf[T](clazz: Class[T]): List[T] = instancesOf[T](allSubTypesOf(clazz).distinct)
	def instancesOfSubtypesOf[T: Manifest]: List[T] = instancesOfSubtypesOf(manifest[T].erasure.asInstanceOf[Class[T]])

	def instantiateOpt[T](clazz: Class[T]): Option[T] = {
		if (isSingleton(clazz)) {Some(getSingleton(clazz).asInstanceOf[T])}
		else {
			val constructorOpt = clazz.getConstructors find {_.getParameterTypes.length == 0}
			constructorOpt.map(c => c.newInstance().asInstanceOf[T])
		}
	}
	def instantiate[T](clazz: Class[T]): T = {
		instantiateOpt[T](clazz) match {
			case None => throw new IllegalArgumentException("No default args constructor for class " + clazz.getName + ", cannot instantiate through reflection assistant")
			case Some(v) => v
		}
	}
	def instantiate[T, U](clazz: Class[T], arg : U): T = {
		val constructorOpt = try {
			Some(clazz.getConstructor(arg.getClass))
		} catch {
			case e : Exception => None
		}

		constructorOpt match {
			case Some(constructor) => constructor.newInstance(arg.asInstanceOf[AnyRef])
			case None => throw new IllegalArgumentException("No default args constructor for class " + clazz.getName + ", cannot instantiate through reflection assistant")
		}
	}

	def instantiate[T](implicit classTag : ClassTag[T]) : T = {
		instantiate[T](classTag.runtimeClass.asInstanceOf[Class[T]])
	}

	def companionFor(clazz : Class[_]) : Option[AnyRef] = {
		try {
			val companionClass = Class.forName(clazz.getTypeName + "$")
			if (isSingleton(companionClass)) {
				Some(getSingleton(companionClass))
			} else {
				None
			}
		} catch {
			case _ : Exception => None
		}
	}

	def instantiateBounded[_, L](clazz: Class[_], lowerBound: Class[L]): L = {
		val constructorOpt = clazz.getConstructors find {_.getParameterTypes.length == 0}
		constructorOpt match {
			case Some(constructor) => constructor.newInstance().asInstanceOf[L]
			case None => if (clazz == lowerBound) {
				throw new IllegalArgumentException("No default args constructor for class " + clazz.getName + ", cannot instantiate through reflection assistant")
			} else {
				instantiateBounded(clazz.getSuperclass, lowerBound)
			}
		}
	}

	def provideInstancesOfSubtypesOf[T: Manifest]: List[T] = {
		allSubTypesOf(manifest[T].erasure).map(provideInstanceOf(_).asInstanceOf[T])
	}
	protected def provideInstanceOf(clazz: Class[_]) = {
		mem_provideInstanceOf(clazz)
	}
	def provideInstanceOf[T: Manifest]: T = {
		//		allSubTypesOf(manifest[T].erasure) match {
		//			case Nil => throw new IllegalArgumentException("No subclasses of " + manifest[T].erasure + " to instantiate")
		//			case head :: _ => instantiate(head.asInstanceOf[Class[T]])
		//		}
		provideInstanceOf(manifest[T].erasure).asInstanceOf[T]
	}
	def provideInstanceOfClass(clazz: Class[_]) = provideInstanceOf(clazz)

	protected var cachedInstances = new mutable.HashMap[Class[_], AnyRef]
	protected val mem_provideInstanceOf = arx.Prelude.memoize((clazz: Class[_]) => {
		val selfClass = if (clazz.isInterface || Modifier.isAbstract(clazz.getModifiers)) {Nil} else {List(clazz)}
		(selfClass ::: allSubTypesOf(clazz)) match {
			case Nil =>
				throw new IllegalArgumentException("No subclasses of " + clazz + " to instantiate")
			case list =>
				val instances = list.map(c => cachedInstances.getOrElseUpdate(c, instantiate(c).asInstanceOf[AnyRef]))
				val sortedInstances = instances.sortBy { i => {
					try {
						val rankMethod = i.getClass.getMethod("injectionRank")
						rankMethod.invoke(i).asInstanceOf[Int]
					} catch {
						case e: Exception => 0
					}
				}
				}
				sortedInstances.last
		}
	})

	def collectAllFieldsFrom(clazz: Class[_]): List[java.lang.reflect.Field] = {
		clazz.getDeclaredFields.toList ::: (clazz.getSuperclass match {
			case null => Nil
			case objclass if (objclass == classOf[Object]) => Nil
			case realClass: Class[_] => collectAllFieldsFrom(realClass)
			case _ => Noto.error("WA?"); Nil
		})
	}

	def fieldType(obj: Any, field: String) = {
		obj.getClass.getMethod(field).getReturnType
	}
	def hasField(obj: Any, field: String) = try {
		obj.getClass.getMethod(field)
		true
	} catch {
		case e: Exception => false
	}
	def getterForField(field : reflect.Field)= {
		try {
			Some(field.getDeclaringClass.getMethod(field.getName))
		} catch {
			case _: NoSuchMethodError => None
		}
	}
	def setterForField(field : reflect.Field) = {
		try {
			Some(field.getDeclaringClass.getMethod(field.getName + "_$eq", field.getType))
		} catch {
			case _ : NoSuchMethodError => None
		}
	}

	def getFieldValue(obj: Any, field: String): Any = {
		obj.getClass.getMethod(field).invoke(obj)
	}
	def getFieldValue(obj: Any, field : reflect.Field) : Any = {
		getFieldValue(obj, field.getName)
	}
	def setFieldValue[T <: AnyRef : Manifest](obj: AnyRef, field: String, value: T) {
		setFieldValue(obj, field, value, manifest[T].runtimeClass)
	}
	def setFieldValue(obj: AnyRef, field: String, value: AnyRef, clazz: Class[_]) {
		try {
			obj.getClass.getMethod(field + "_$eq", clazz).invoke(obj, value)
		} catch {
			case e: Exception =>
				Noto.error(f"Attempted to reflectively set field value $field to $value on $obj, but could not find method")
		}
	}

	def toScalaType(clazz : Class[_]) : ClassSymbol = {
		mirror.staticClass(clazz.getCanonicalName)
	}

	def reflectMethod[T](inst : T, methodSymbol : MethodSymbol)(implicit classTag : ClassTag[T]) = mirror.reflect(inst).reflectMethod(methodSymbol)
	def invoke[T](inst : T, methodSymbol : MethodSymbol)(args: Any*)(implicit classTag : ClassTag[T]) = reflectMethod(inst, methodSymbol)(classTag)(args: _*)
}
