package arx.engine.control.components.windowing

import arx.application.Noto
import arx.core.introspection.ReflectionAssistant
import arx.core.macros.GenerateCompanion
import arx.core.representation.ConfigValue
import arx.engine.control.components.windowing.widgets.DynamicWidgetData
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.data.ConfigDataLoader
import arx.resource.ResourceManager

trait WidgetPrototype {
	def instantiate(ws : WindowingSystem) : Widget
	def reload(w : Widget) : Unit
	def load(w : Widget) : Unit = reload(w)
}

object WidgetPrototype {
	val Sentinel : WidgetPrototype = new WidgetPrototype {
		override def instantiate(ws: WindowingSystem): Widget = {
			ws.createWidget()
		}
		override def reload(w: Widget): Unit = {}
	}

	def fromConfig(resourcePath : String, key : String) = {
		var effResourcePath = if (resourcePath.endsWith(".sml")) {
			resourcePath
		} else {
			resourcePath + ".sml"
		}
		effResourcePath = if (effResourcePath.startsWith("widgets/")) {
			effResourcePath
		} else {
			"widgets/" + effResourcePath
		}
		new SMLWidgetPrototype(() => ResourceManager.sml(effResourcePath).field(key))
	}
}

class SMLWidgetPrototype(configFunc : () => ConfigValue) extends WidgetPrototype {
	/**
	 * Instantiates the base information for the widget, but does not load the actual prototyped data. Allows for
	 * a two phase initialization such that children of a widget can reference one another
	 */
	override def instantiate(ws : WindowingSystem): Widget = {
		ReflectionAssistant.ensureReflectionsLoaded()

		val config = configFunc()
		if (! config.hasField("type")) {
			Noto.warn("Loading widget from sml that does not have a type specified")
		}
		val widgetType = config.field("type").strOrElse("SimpleWidget")
		val w = ws.createWidget()
		WidgetType.types(widgetType).initializeWidget(w)

		w.data[WidgetPrototypeData].prototype = this

		w
	}

	override def reload(w: Widget): Unit = {
		val config = configFunc()

		val childFields = config.field("children").fields
		val childIdentifiers = childFields.keys.toSet

		val childrenToDelete = w.children.filterNot(w => w.notConfigManaged || w.configIdentifier.forall(ci => childIdentifiers.contains(ci)))
		if (childrenToDelete.nonEmpty) {
			Noto.info(s"deleting ${childrenToDelete.size} children")
		}
		childrenToDelete.foreach(w => w.destroy())
		w.dataOpt[DynamicWidgetData].foreach(dyn => dyn.forceRecomputation = true)
//		val childrenToUpdate = w.children.filter(w => w.configIdentifier.exists(ci => childIdentifiers.contains(ci)))
//		childrenToUpdate.foreach(w => reload(w))
//		if (childrenToUpdate.nonEmpty) { Noto.info(s"updating ${childrenToUpdate.size} children")}
		val childrenToCreate = childFields.filter(t => ! w.children.exists(w => w.configIdentifier.contains(t._1)))
		val createdChildren = childrenToCreate.map {
			case (configIdent, configValue) => {
				if (configValue.isStr) {
					None -> w.createChild(configValue.str)
				} else {
					val prototype = new SMLWidgetPrototype(() => configFunc().field("children").fields(configIdent))
					val child = prototype.instantiate(w.windowingSystem)
					child.parent = w
					child.widgetData.configIdentifier = Some(configIdent)
					Some(prototype) -> child
				}
			}
		}

		w.allData.foreach {
			case wad : TWidgetAuxData =>
				if (wad.autoLoadSimpleValuesFromConfig) {
					ConfigDataLoader.loadSimpleValuesFromConfig(wad, config)
				}
				wad.loadFromConfig(w, config, reload = false)
			case _ => // do nothing for not-widget data
		}

		createdChildren.foreach { case (Some(prototype), c) => prototype.load(c); case _ => }

		w.children
	}
}

@GenerateCompanion
class WidgetPrototypeData extends TWidgetAuxData {
	var prototype : WidgetPrototype = WidgetPrototype.Sentinel
}