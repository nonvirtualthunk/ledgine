package arx.engine.graphics.components.windowing

/**
 * TODO: Add javadoc
 */

import java.util.concurrent.atomic.AtomicLong

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.{Watcher, Watcher2, Watcher3}
import arx.core.introspection.ReflectionAssistant
import arx.core.math.{Rectf, Recti}
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.DimensionExpression.ExpandTo
import arx.engine.control.components.windowing.widgets._
import arx.engine.control.components.windowing.widgets.data.{DrawingData, TWidgetAuxData}
import arx.engine.control.data.WindowingControlData
import arx.engine.graphics.components.{DrawPriority, GraphicsComponent}
import arx.engine.graphics.components.windowing.WindowingGraphicsComponent.WidgetWatchers
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.helpers.{Color, RGBA}
import arx.graphics.{AVBO, Axis, GL, VBO}
import arx.engine.graphics.data.WindowingSystemAttributeProfile._
import arx.engine.graphics.event.WidgetDestroyedGraphicsEvent
import arx.engine.world.World
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11

import scala.collection.mutable
import scala.language.postfixOps


class WindowingGraphicsComponent extends GraphicsComponent {
	lazy val shader = ResourceManager.shader("shaders/windowing/main")


	//	WD.textureBlock.magFilter = GL11.GL_LINEAR
	//	WD.textureBlock.minFilter = GL11.GL_LINEAR

	var renderers : List[WindowingRenderer] = Nil

	val watchers = new mutable.HashMap[Widget, WidgetWatchers]()

	val updateRevision = new AtomicLong(1L)
	val solidifiedRevision = new AtomicLong(0L)

	var renderedViewport = GL.viewport

	val viewportWatcher = new Watcher(renderedViewport)

	var customVBOs = List[AVBO]()
	var needsRedraw = true


	override def drawPriority: DrawPriority = DrawPriority.Final

	def createWatchers(widget: Widget) = WidgetWatchers(Watcher(widget.position), Watcher(widget.z), Watcher(widget.dimensions), Watcher(widget.showing.resolve()))


	override protected def onInitialize(game: World, display: World): Unit = {
		renderers = ReflectionAssistant.allSubTypesOf[WindowingRenderer]
			.map(c => ReflectionAssistant.instantiate(c, display.worldData[WindowingGraphicsData]))
			.sortBy(c => c.drawPriority)

		onGraphicsEvent {
			case WidgetDestroyedGraphicsEvent(widget) =>
				watchers.remove(widget)
		}
	}

	override def draw(game: World, graphics: World): Unit = {
		val WD = graphics.worldData[WindowingGraphicsData]
		renderedViewport = GL.viewport

		shader.bind()
		shader.setUniform("mainTexture", 0)

		val vbo = WD.vbo

		WD.pov.look()

		WD.textureBlock.bind(0)

		GL.glSetState(GL11.GL_DEPTH_TEST, false)

		val cur = updateRevision.get()
		if (vbo.solidifyIfNecessary()) {
			solidifiedRevision.set(cur)
			// if we have been modified again since the point at which we started solidifying, mark for re-drawing
			if (solidifiedRevision.get() < updateRevision.get()) {
				vbo.state.set(VBO.Dirty)
			}
		}
		vbo.drawElements(GL11.GL_TRIANGLES)

		customVBOs.foreach(cvbo => {
			cvbo.solidifyIfNecessary()
			cvbo.drawElements(GL11.GL_TRIANGLES)
		})

		GL.glSetState(GL11.GL_DEPTH_TEST, true)
		needsRedraw = false
	}


	val actuallyChangedCounter = Metrics.counter("WindowingGraphicsComponent.anyChangeCount")
	val notChangedCounter = Metrics.counter("WindowingGraphicsComponent.noChangeCount")

	val updateTxn = Metrics.transaction("WindowingGraphicsComponent.onUpdate")

	override protected def onUpdate(game: World, graphics: World, dt: UnitOfTime, time: UnitOfTime): Unit = {
		val txn = updateTxn.start()

		val WD = graphics.worldData[WindowingGraphicsData]
		val vbo = WD.vbo

		txn.timeSegment("windowingSystem.update") {
			WD.desktop.windowingSystem.update()
		}

		WD.desktop.synchronized {
			var anyChanged = false
			txn.timeSegment("check for changes") {
				if (viewportWatcher.hasChanged) {
					WD.desktop.width = DimensionExpression.Constant(GL.viewport.width)
					WD.desktop.height = DimensionExpression.Constant(GL.viewport.height)
					anyChanged = true
				}
				if (checkForWidgetChanges(WD.desktop)) {
					anyChanged = true
				}
			}


			if (anyChanged) {
				txn.timeSegment("update resolved widget variables") {
					actuallyChangedCounter.inc()
					updateRevision.incrementAndGet()
					if (!vbo.changeState(AVBO.Updated, AVBO.Dirty) && !vbo.changeState(AVBO.Clean, AVBO.Dirty)) {
						Noto.info(s"Could not change to dirty on update, currently is : ${vbo.state.get()}")
					}

					for (axis <- Axis.XYZ) {
						updateResolvedWidgetVariablesForAxis(WD.desktop, axis, Set(), new mutable.HashSet())
					}
				}

				txn.timeSegment("precompute mositions, unmark") {
					precomputeAbsolutePosition(WD.desktop)
					unmarkAllModified(WD.desktop)
				}

				txn.timeSegment("update vbo") {
					if (vbo.changeState(AVBO.Dirty, AVBO.Updating)) {
						vbo.softClear()

						customVBOs = Nil
						updateWindowingDrawData(WD, WD.desktop, Recti(0, 0, GL.viewport.w, GL.viewport.h), RGBA(1, 1, 1, 1))
						vbo.state.set(VBO.Updated)
						needsRedraw = true
					}
				}
			} else {
				notChangedCounter.inc()
			}
		}

		txn.end()
	}

	def unmarkAllModified(w : Widget): Unit = {
		w.unmarkModified()
		w.children.foreach(unmarkAllModified)
	}

//	val colors = Color(255, 255, 255, 255) :: Color(255, 0, 0, 255) :: Color(0, 255, 0, 255) :: Color(255, 255, 0, 255) :: Nil
	val fixedBothAxes = Vec2T(true,true)

	var windowDrawRecurseDepth = 0
	var colors = List(Color.Red, Color.Green, Color.Blue, Color(128,255,128,255), Color(255,128,255,255), Color(255,255,128,255), Color(128,128,128,255), Color(128, 128, 255, 255), Color(255, 128, 128), Color(255,0,255,255))

	def updateWindowingDrawData(WD : WindowingGraphicsData, w: Widget, bounds: Recti, tintIn : RGBA): Unit = {
		val vbo = WD.vbo
		val textureBlock = WD.textureBlock
		val tint = tintIn * w.drawing.tintColor.resolve().getOrElse(RGBA.White)

		if (!w.showing) {
			return
		}
		windowDrawRecurseDepth += 1

		val absPos = w.drawing.absolutePosition
		val relPos = w.drawing.relativePosition
//		Noto.info(s"Widget ${w.effectiveIdentifier} bounds $bounds and relPos $relPos, effDim ${w.drawing.effectiveDimensions} {")
		Noto.increaseIndent()

		def renderQuads(quads: Traversable[WQuad]): Unit = {
			for (quad <- quads) {
				val ii = vbo.incrementIndexOffset(6)
				val vi = vbo.incrementVertexOffset(4)
				vbo.setA(V, vi + 0, absPos.x + quad.rect.minX, absPos.y + quad.rect.minY)
				vbo.setA(V, vi + 1, absPos.x + quad.rect.minX, absPos.y + quad.rect.maxY)
				vbo.setA(V, vi + 2, absPos.x + quad.rect.maxX, absPos.y + quad.rect.maxY)
				vbo.setA(V, vi + 3, absPos.x + quad.rect.maxX, absPos.y + quad.rect.minY)

				val imgRect = textureBlock.getOrElseUpdateRectFor(quad.image)
				val tpos = imgRect.xy + quad.subRect.xy * imgRect.dimensions
				val tdim = imgRect.dimensions * quad.subRect.dimensions
				val tcs = quad.texCoords match {
					case Some(rawTcs) => rawTcs
					case None => Array(Vec2f(tpos.x, tpos.y),
						Vec2f(tpos.x + tdim.x, tpos.y),
						Vec2f(tpos.x + tdim.x, tpos.y + tdim.y),
						Vec2f(tpos.x, tpos.y + tdim.y))
				}

				if (quad.rotation != 0) {
					val toff = 3 + quad.rotation / 90
					for (q <- 0 until 4) {
						vbo.setA(C, vi + q, quad.color.asRGBA * tint)
						if (tcs((q + toff) % 4) == null) {
							println("BAD")
						}
						vbo.setA(TC, vi + q, tcs((q + toff) % 4))
						vbo.setA(B, vi + q, bounds.x, bounds.y, bounds.x + bounds.w, bounds.y + bounds.h)
					}
				} else if (quad.flipX || quad.flipY) {
					val minTC = tcs(0)
					val maxTC = tcs(2)

					for (q <- 0 until 4) {
						val tc = tcs((q + 3) % 4)
						var tcx = tc.x
						var tcy = tc.y

						if (quad.flipX) {
							tcx = minTC.x + (maxTC.x - tcx)
						}
						if (quad.flipY) {
							tcy = minTC.y + (maxTC.y - tcy)
						}

						vbo.setA(C, vi + q, quad.color.asRGBA * tint)
						vbo.setA(TC, vi + q, tcx, tcy)
						vbo.setA(B, vi + q, bounds.x, bounds.y, bounds.x + bounds.w, bounds.y + bounds.h)
					}
				} else {
					for (q <- 0 until 4) {
						vbo.setA(C, vi + q, quad.color.asRGBA * tint)
						vbo.setA(TC, vi + q, tcs((q + 3) % 4))
						vbo.setA(B, vi + q, bounds.x, bounds.y, bounds.x + bounds.w, bounds.y + bounds.h)
					}
				}

				//			Noto.info(s"Drawing quad: $quad")
				vbo.setIQuad(ii, vi)
			}
		}


		val selfArea = Recti(0,0,w.drawing.effectiveDimensions.x, w.drawing.effectiveDimensions.y)
		renderers.foreach(r => {
			renderQuads(r.render(w, beforeChildren = true, selfArea))
			r.renderRaw(vbo, textureBlock, selfArea, bounds.xy + relPos.xy)(w, beforeChildren = true)
			for (axis <- Axis.XY) {
				r.modifyBounds(w, axis, fixedOnAxis = true, selfArea, w.drawing.effectiveDimensions)
			}
			r.renderCustomVBO(textureBlock, selfArea, bounds.xy + relPos.xy)(w) match {
				case Some(vbo) => customVBOs ::= vbo
				case None =>
			}
		})

		val pos = w.drawing.absolutePosition + Vec3i(w.drawing.clientOffset, 0)
		val size = w.drawing.clientDim
		val newBounds = bounds.intersect(Recti(pos.x, pos.y, size.x, size.y))

//		renderQuads(WQuad(Rectf(bounds), ResourceManager.blankImage, colors(windowDrawRecurseDepth % colors.size).asRGBA * RGBA(1.0f,1.0f,1.0f,0.5f)) :: Nil)

//		if (w.effectiveIdentifier == "AttackList") {
//			println("attack list")
//		}

		for (child <- w.children.sortBy(_.drawing.absolutePosition.z)) {
			updateWindowingDrawData(WD, child, newBounds, tint)
		}

		selfArea.x = 0
		selfArea.y = 0
		selfArea.width = w.drawing.effectiveDimensions.x
		selfArea.height = w.drawing.effectiveDimensions.y
		renderers.foreach(r => {
			renderQuads(r.render(w, beforeChildren = false, selfArea))
			r.renderRaw(vbo, textureBlock, selfArea, bounds.xy + relPos.xy)(w, beforeChildren = false)
			for (axis <- Axis.XY) {
				r.modifyBounds(w, axis, fixedOnAxis = true, selfArea, w.drawing.effectiveDimensions)
			}
		})

		Noto.decreaseIndent()
//		Noto.info(s"} end ${w.effectiveIdentifier}")

		windowDrawRecurseDepth -= 1
	}

	val standardSize = Vec2i(10, 10)

	def calculateIntrinsicDimFor(w: Widget, fixedX: Option[Int], fixedY: Option[Int]) = {
		renderers.findFirstWith(r => r.intrinsicSize(w, fixedX, fixedY)) match {
			case Some((renderer, size)) => size
			case None => standardSize
		}
	}

	def checkForWidgetChanges(widget: Widget): Boolean = {
		val watch = watchers.getOrElseUpdate(widget, createWatchers(widget))

		val widgetModified = widget.isModified
		val ret = widgetModified ||
			(if (watch.first || watch.anyChanged) {
				watch.first = false
				true
			} else {
				false
			})

		// have to check all children to ensure that their watchers get updated
		val anyChildModified = widget.children.exists(checkForWidgetChanges)
		ret || anyChildModified
	}


	def resolveEffectiveDimensions(widget: Widget, axis : Axis) = {
		var ret = 0
		var fixedOnAxis = false
		val maxDim = widget.widgetData.maximumDimensions(axis).flatMap(d => resolveFixedDimensionFor(widget, d, axis))
		for (dim <- resolveFixedDimensionFor(widget, widget.dimensions(axis), axis)) {
			ret = dim
			fixedOnAxis = true
		}

		val inPad = widget.drawing.interiorPadding
		widget.dimensions(axis) match {
			case DimensionExpression.Intrinsic =>
				val intr = calculateIntrinsicDimFor(widget, resolveFixedDimensionFor(widget, widget.dimensions(Axis.X), Axis.X), resolveFixedDimensionFor(widget, widget.dimensions(Axis.Y), Axis.Y))
				ret = intr(axis) + inPad(axis) * 2
			case DimensionExpression.WrapContent =>
				var min = 0
				var max = 0
				widget.children.foreach(w => {
					val rpos = w.drawing.relativePosition
					val edim = w.drawing.effectiveDimensions
					min = min.min(rpos(axis))
					max = max.max(rpos(axis) + edim(axis))
				})
				ret = max - min + inPad(axis) * 2
			case _ => // do nothing further
		}

		ret = ret.min(maxDim.getOrElse(Int.MaxValue))

		val dims = Vec2i(if (axis == Axis.X) { ret } else { 0 }, if (axis == Axis.Y) { ret } else { 0 })
		val clientArea = Recti(inPad.x,inPad.y,dims.x, dims.y)
		for (render <- renderers) {
			render.modifyBounds(widget, axis, fixedOnAxis, clientArea, dims)
		}

		(dims(axis), clientArea.axis(axis))
	}

	def resolveFixedDimensionFor(widget: Widget, expr : DimensionExpression, axis: Int): Option[Int] = {
		expr match {
			case DimensionExpression.Constant(constValue) =>
				Some(constValue)
			case DimensionExpression.Proportional(proportion) =>
				Some((widget.parent.drawing.clientDim(axis) * proportion).round)
			case DimensionExpression.Relative(delta) =>
				Some(widget.parent.drawing.clientDim(axis) + delta)
			case DimensionExpression.ExpandToParent =>
				Some( widget.parent.drawing.clientDim(axis) - widget.drawing.relativePosition(axis))
			case ExpandTo(sibling) =>
				Some(sibling.drawing.relativePosition(axis) - widget.drawing.relativePosition(axis))
			case _ => None
		}
	}

	def resolveRelativePosition(widget: Widget, axis : Axis) = {
		(widget.position(axis) match {
			case PositionExpression.Constant(constValue, relativeTo) =>
				if (relativeTo == TopLeft || (axis == Axis.X && relativeTo == BottomLeft) || (axis == Axis.Y && relativeTo == TopRight)) {
					constValue
				} else {
					widget.parent.drawing.clientDim(axis) - widget.drawing.effectiveDimensions(axis) - constValue
				}
			case PositionExpression.Proportional(p, relativeTo, anchorTo) =>
				val offsetExpr = widget.parent.drawing.clientDim(axis) * p
				val selectionPoint = if (relativeTo == TopLeft || (axis == Axis.X && relativeTo == BottomLeft) || (axis == Axis.Y && relativeTo == TopRight)) {
					offsetExpr
				} else {
					widget.parent.drawing.clientDim(axis) - widget.drawing.effectiveDimensions(axis) - offsetExpr
				}

				anchorTo match {
					case TopLeft =>
						selectionPoint
					case Center =>
						selectionPoint - widget.drawing.effectiveDimensions(axis) * 0.5f
					case other =>
						Noto.error(s"Anchor points other than [TopLeft,Center] not yet supported: $other")
						selectionPoint
				}
			case PositionExpression.Centered =>
				(widget.parent.drawing.clientDim(axis) - widget.drawing.effectiveDimensions(axis)) / 2
			case PositionExpression.Relative(relativeTo, offset, direction) =>
				val baseRelPos = if (relativeTo.parent == widget.parent) {
					relativeTo.drawing.relativePosition
				} else {
					Noto.error("Temporarily disabled positioning relative to widget that does not share parent")
					relativeTo.drawing.relativePosition
//					relativeTo.drawing.absolutePosition - widget.parent.drawing.absolutePosition - Vec3i(widget.parent.drawing.clientOffset,0)
				}

				direction match {
					case Cardinals.Right if axis == Axis.X =>
						baseRelPos.x + relativeTo.drawing.effectiveDimensions.x + offset
					case Cardinals.Left if axis == Axis.X =>
						baseRelPos.x - widget.drawing.effectiveDimensions.x - offset
					case Cardinals.Down if axis == Axis.Y =>
						baseRelPos.y + relativeTo.drawing.effectiveDimensions.y + offset
					case Cardinals.Up if axis == Axis.Y =>
						baseRelPos.y - widget.drawing.effectiveDimensions.y - offset
					case Cardinals.Center =>
						val dimDiff = relativeTo.drawing.effectiveDimensions(axis) - widget.drawing.effectiveDimensions(axis)
						baseRelPos(axis) + dimDiff / 2
					case _ =>
						Noto.error("Unsupported relative widget position direction/axis: " + direction + "/" + axis)
						0
				}
			case PositionExpression.Match(matchTo) =>
				if (matchTo.parent == widget.parent) {
					matchTo.drawing.relativePosition(axis)
				} else {
					Noto.error("Temporarily disabled match positioning relative to widget that does not share parent")
					???
//					matchTo.drawing.absolutePosition(axis) - (widget.parent.drawing.absolutePosition(axis) + widget.parent.drawing.clientOffset(axis))
				}
			case PositionExpression.Absolute(value, relativeTo) =>
				val raw = if (widget.parent.isSentinel) {
					value
				} else {
					value - widget.parent.drawing.absolutePosition(axis)
				}

				if ((axis == Axis.Y && (relativeTo == BottomLeft || relativeTo == BottomRight)) || (axis == Axis.X && (relativeTo == TopRight || relativeTo == BottomRight))) {
					raw - widget.drawing.effectiveDimensions(axis)
				} else {
					raw
				}
			case PositionExpression.Flow =>
				0
			// do nothing there, this is considered unconstrained, will be filled in
			// more intelligently by the remaining layout engine...unless we deal with it
			// here, which we could
		}).round
	}

	def updateResolvedWidgetVariablesForAxis(w: Widget, axis : Axis, activeSet : Set[(Widget, Axis)],  resolved: mutable.Set[(Widget, Axis)]): Unit = {
		if (w.showing) {
			val DD = w[DrawingData]
			if (axis == Axis.Z) {
				DD.relativePosition = DD.relativePosition.withAxisSetTo(Axis.Z,w.z)
				DD.absolutePosition = DD.absolutePosition.withAxisSetTo(Axis.Z,w.z)
			} else {
				if (resolved.contains(w -> axis)) { // if it's already resolved nothing else to do
					return
				}
				val dependsOn = w.position(axis).dependsOn(w, axis) ::: w.dimensions(axis).dependsOn(w, axis, renderers)
				val remainingDepends = dependsOn.filterNot(resolved)
				if (remainingDepends.exists(d => activeSet.contains(d))) {
					//				Noto.info(s"${w.effectiveIdentifier} saw a recursive dependency, breaking")
					// if this is dependent on something that is actively being resolved then we have a recursive dependency. Break it here
					return
				}

				remainingDepends.foreach(d => updateResolvedWidgetVariablesForAxis(d._1, d._2, activeSet + (w -> axis), resolved))

				val (effDim, effClientArea) = resolveEffectiveDimensions(w, axis)

				DD.effectiveDimensions = Vec2i(if (axis == Axis.X) {
					effDim
				} else {
					DD.effectiveDimensions.x
				}, if (axis == Axis.Y) {
					effDim
				} else {
					DD.effectiveDimensions.y
				})
				DD.effectiveClientArea = DD.effectiveClientArea.withAxisSetTo(axis, effClientArea)

				if (w.parent != null && w.parent.notSentinel) {
					DD.relativePosition = DD.relativePosition.withAxisSetTo(axis, resolveRelativePosition(w, axis))
				}

				resolved += (w -> axis)
			}

			for (child <- w.children) {
				updateResolvedWidgetVariablesForAxis(child, axis, activeSet + (child -> axis), resolved)
			}
		} else {
			resolved += (w -> axis)
		}
	}

	def precomputeAbsolutePosition(w : Widget): Unit = {
		val DD = w.drawing
		if (w.parent != null && w.parent.notSentinel) {
			DD.absolutePosition = w.parent.drawing.absolutePosition + Vec3i(w.parent.drawing.clientOffset, 0) + DD.relativePosition
		}
		w.children.foreach(c => precomputeAbsolutePosition(c))
	}
}

object WindowingGraphicsComponent {

	case class WidgetWatchers(posWatcher: Watcher2[PositionExpression],
									  zWatcher : Watcher[Int],
									  dimWatcher: Watcher2[DimensionExpression],
									  showingWatcher: Watcher[Boolean]) {
		var first = true

		def peekAnyChanged = posWatcher.peekChanged || dimWatcher.peekChanged || showingWatcher.peekChanged

		def anyChanged = posWatcher.hasChanged || dimWatcher.hasChanged || showingWatcher.hasChanged
	}

}