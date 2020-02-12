package arx.engine.control.components.windowing.widgets

import arx.core.representation.ConfigValue
import arx.engine.control.components.windowing.{Widget, WidgetInstance, WidgetType}
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.data.Moddable
import arx.graphics.helpers.{Color, RGBA, RichText}
import arx.Prelude._
import arx.application.Noto
import arx.core.NoAutoLoad
import arx.engine.control.event.{MouseButton, MouseReleaseEvent}

class TabWidget(val widget: Widget) extends WidgetInstance

object TabWidget extends WidgetType[TabWidget, TabWidgetData] {
	override def initializeWidget(widget: Widget): TabWidget = {
		widget.attachData[TabWidgetData]
		new TabWidget(widget)
	}
}

class TabWidgetData extends TWidgetAuxData {
	@NoAutoLoad var tabs = Vector[Widget]()
	@NoAutoLoad var activeTab: Widget = _
	@NoAutoLoad var tabHeadings = Vector[Widget]()
	var tabHeight = 50

	override def loadFromConfig(widget: Widget, mainConf: ConfigValue, reload: Boolean): Unit = {
		tabHeadings.foreach(t => t.destroy())
		tabHeadings = Vector()
		tabs = Vector()
		//		if (!reload) {
		var tabPairs: Vector[(String, String)] = Vector()
		for (tabsConf <- mainConf.fieldOpt("tabs")) {
			if (tabsConf.isObj) {
				tabPairs ++= tabsConf.fields.map { case (k, v) => k -> v.str }.toList
			}

			for (tabSubArr <- tabsConf.arrOpt ; tabSubObj <- tabSubArr) {
				tabPairs :+= (tabSubObj.field("heading").str -> tabSubObj.field("tab").str)
			}
		}

		for ((tabTxt, tabId) <- tabPairs) {
			for (tab <- widget.childWithIdentifier(tabId)) {
				tab.y = PositionExpression.Constant(tabHeight)
				tab.width = DimensionExpression.MatchParent
				tab.height = DimensionExpression.Relative(-tabHeight)
				tab.showing = Moddable(() => {
					activeTab == tab
				})
				tabs :+= tab
				if (activeTab == null) {
					activeTab = tab
				}

				val tabHeadingPath = mainConf.fieldOpt("tabHeading").map(_.str).getOrElse("DefaultWidgets.TabHeading")
				val tabHeading = widget.createChild(tabHeadingPath)
				tabHeading.notConfigManaged = true

				for (td <- tabHeading.dataOpt[TextDisplay]) {
					td.text = Moddable(RichText(tabTxt))
				}

				tabHeading.consumeEvent {
					case MouseReleaseEvent(MouseButton.Left, _, _) =>
						activeTab = tab
						tab.markModified()
				}
				tabHeading.drawing.backgroundColor = Moddable(() => if (tabHeading.isUnderPress) {
					RGBA(0.35f, 0.35f, 0.35f, 1.0f)
				} else {
					Color.White
				})

				tabHeadings :+= tabHeading
			}
		}

		tabHeadings.foreach(a => {
			a.width = DimensionExpression.Proportional(1.0f / tabs.size)
			a.height = DimensionExpression.Constant(tabHeight)
		})
		tabHeadings.toList.sliding2.foreach { case (a, b) =>
			b.x = PositionExpression.Relative(a, 0)
		}
	}

	//		}

}