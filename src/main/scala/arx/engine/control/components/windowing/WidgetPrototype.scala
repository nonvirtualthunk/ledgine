package arx.engine.control.components.windowing

import arx.core.introspection.ReflectionAssistant
import arx.core.macros.GenerateCompanion
import arx.core.representation.ConfigValue
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.data.ConfigDataLoader
import arx.resource.ResourceManager

trait WidgetPrototype {
	def instantiate(ws : WindowingSystem) : Widget
	def reload(w : Widget)
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
	override def instantiate(ws : WindowingSystem): Widget = {
		ReflectionAssistant.ensureReflectionsLoaded()

		val config = configFunc()
		val widgetType = config.field("type").strOrElse("SimpleWidget")
		val w = ws.createWidget()
		WidgetType.types(widgetType).initializeWidget(w)

		reload(w)

		w.data[WidgetPrototypeData].prototype = this

		w
	}

	override def reload(w: Widget): Unit = {
		val config = configFunc()
		w.allData.foreach {
			case wad : TWidgetAuxData =>
				if (wad.autoLoadSimpleValuesFromConfig) {
					ConfigDataLoader.loadSimpleValuesFromConfig(wad, config)
				}
				wad.loadFromConfig(w, config, reload = false)
			case _ => // do nothing for not-widget data
		}

		val childFields = config.field("children").fields
		val childIdentifiers = childFields.keys.toSet

		val childrenToDelete = w.children.filterNot(w => w.configIdentifier.forall(ci => childIdentifiers.contains(ci)))
		childrenToDelete.foreach(w => w.destroy())
		val childrenToUpdate = w.children.filter(w => w.configIdentifier.exists(ci => childIdentifiers.contains(ci)))
		childrenToUpdate.foreach(w => reload(w))
		val childrenToCreate = childFields.filter(t => ! w.children.exists(w => w.configIdentifier.contains(t._1)))
		childrenToCreate.foreach {
			case (configIdent, _) => {
				val prototype = new SMLWidgetPrototype(() => configFunc().field("children").fields(configIdent))
				val child = prototype.instantiate(w.windowingSystem)
				child.parent = w
				child.widgetData.configIdentifier = Some(configIdent)
			}
		}

		w.children
	}
}

@GenerateCompanion
class WidgetPrototypeData extends TWidgetAuxData {
	var prototype : WidgetPrototype = WidgetPrototype.Sentinel
}