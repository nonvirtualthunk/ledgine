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
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets._
import arx.engine.control.components.windowing.widgets.data.{DrawingData, TWidgetAuxData}
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.graphics.components.windowing.WindowingGraphicsComponent.WidgetWatchers
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.helpers.Color
import arx.graphics.{AVBO, GL, VBO}
import arx.engine.graphics.data.WindowingSystemAttributeProfile._
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

	def createWatchers(widget: Widget) = WidgetWatchers(Watcher(widget.position), Watcher(widget.dimensions), Watcher(widget.showing.resolve()))


	override protected def onInitialize(game: World, display: World): Unit = {
		renderers = ReflectionAssistant.allSubTypesOf[WindowingRenderer]
			.map(c => ReflectionAssistant.instantiate(c, display.worldData[WindowingGraphicsData]))
			.sortBy(c => c.drawPriority)
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


	override protected def onUpdate(game: World, graphics: World, dt: UnitOfTime): Unit = {
		val WD = graphics.worldData[WindowingGraphicsData]
		val vbo = WD.vbo
		WD.desktop.synchronized {
			var anyChanged = false
			if (viewportWatcher.hasChanged) {
				WD.desktop.width = DimensionExpression.Constant(GL.viewport.width)
				WD.desktop.height = DimensionExpression.Constant(GL.viewport.height)
				anyChanged = true
			}
			anyChanged ||= checkForWidgetChanges(WD.desktop)

			if (anyChanged) {
				updateRevision.incrementAndGet()
				if (!vbo.changeState(VBO.Updated, VBO.Dirty) && !vbo.changeState(VBO.Clean, VBO.Dirty)) {
					Noto.info(s"Could not change to dirty on update, currently is : ${vbo.state.get()}")
				}

				updateResolvedWidgetVariables(WD.desktop, new mutable.HashSet[Widget]())

				if (vbo.changeState(VBO.Dirty, VBO.Updating)) {
					vbo.softClear()
					// could if(anyChanged) here
					customVBOs = Nil
					updateWindowingDrawData(WD, WD.desktop, Recti(0, 0, GL.viewport.w, GL.viewport.h))
					vbo.state.set(VBO.Updated)
					needsRedraw = true
				}
			}
		}
	}

	val colors = Color(255, 255, 255, 255) :: Color(255, 0, 0, 255) :: Color(0, 255, 0, 255) :: Color(255, 255, 0, 255) :: Nil
	val fixedBothAxes = Vec2T(true,true)

	def updateWindowingDrawData(WD : WindowingGraphicsData, w: Widget, bounds: Recti): Unit = {
		val vbo = WD.vbo
		val textureBlock = WD.textureBlock

		if (!w.showing) {
			return
		}

		val relPos = w.drawing.relativePosition
		//		Noto.info(s"Widget ${w.identifier} of class ${w.getClass.getSimpleName} bounds $bounds and relPos $relPos")
		Noto.indentation += 1

		def renderQuads(quads: Traversable[WQuad]): Unit = {
			for (quad <- quads) {
				val ii = vbo.incrementIndexOffset(6)
				val vi = vbo.incrementVertexOffset(4)
				vbo.setA(V, vi + 0, bounds.x + relPos.x + quad.rect.minX, bounds.y + relPos.y + quad.rect.minY)
				vbo.setA(V, vi + 1, bounds.x + relPos.x + quad.rect.minX, bounds.y + relPos.y + quad.rect.maxY)
				vbo.setA(V, vi + 2, bounds.x + relPos.x + quad.rect.maxX, bounds.y + relPos.y + quad.rect.maxY)
				vbo.setA(V, vi + 3, bounds.x + relPos.x + quad.rect.maxX, bounds.y + relPos.y + quad.rect.minY)

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
						vbo.setA(C, vi + q, quad.color.asRGBA)
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

						vbo.setA(C, vi + q, quad.color.asRGBA)
						vbo.setA(TC, vi + q, tcx, tcy)
						vbo.setA(B, vi + q, bounds.x, bounds.y, bounds.x + bounds.w, bounds.y + bounds.h)
					}
				} else {
					for (q <- 0 until 4) {
						vbo.setA(C, vi + q, quad.color.asRGBA)
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
			r.modifyBounds(w, fixedBothAxes, selfArea, w.drawing.effectiveDimensions)
			r.renderCustomVBO(textureBlock, selfArea, bounds.xy + relPos.xy)(w) match {
				case Some(vbo) => customVBOs ::= vbo
				case None =>
			}
		})

		//		renderQuad(WQuad(Rectf(-relPos.x,-relPos.y,bounds.width,bounds.height), "default/blank_bordered.png", colors(Noto.indentation)))

		val pos = w.drawing.absolutePosition + Vec3i(w.drawing.clientOffset, 0)
		val size = w.drawing.clientDim
		val newBounds = bounds.intersect(Recti(pos.x, pos.y, size.x, size.y))

		for (child <- w.children) {
			updateWindowingDrawData(WD, child, newBounds)
		}

		selfArea.x = 0
		selfArea.y = 0
		selfArea.width = w.drawing.effectiveDimensions.x
		selfArea.height = w.drawing.effectiveDimensions.y
		renderers.foreach(r => {
			renderQuads(r.render(w, beforeChildren = false, selfArea))
			r.renderRaw(vbo, textureBlock, selfArea, bounds.xy + relPos.xy)(w, beforeChildren = false)
			r.modifyBounds(w, fixedBothAxes, selfArea, w.drawing.effectiveDimensions)
		})

		Noto.indentation -= 1
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

		val anyChildModified = widget.children.exists(checkForWidgetChanges)
		ret || anyChildModified
	}

	def resolveEffectiveDimensions(widget: Widget) = {
		val fixedOnAxis = Vec2T(false, false)
		val ret = Vec2i(0, 0)
		for (axis <- 0 until 2) {
			resolveFixedDimensionFor(widget, axis) match {
				case Some(v) =>
					ret(axis) = v
					fixedOnAxis(axis) = true
				case None => // do nothing
			}
		}

		for (axis <- 0 until 2) {
			widget.dimensions(axis) match {
				case DimensionExpression.Intrinsic =>
					ret(axis) = calculateIntrinsicDimFor(widget, if (fixedOnAxis.x) {
						Some(ret(0))
					} else {
						None
					}, if (fixedOnAxis.y) {
						Some(ret(1))
					} else {
						None
					})(axis)
				case _ => // do nothing further
			}
		}

		val clientArea = Recti(0,0,ret(0),ret(1))
		for (render <- renderers) {
			render.modifyBounds(widget, fixedOnAxis, clientArea, ret)
		}

		(ret, clientArea)
	}

	def resolveFixedDimensionFor(widget: Widget, axis: Int): Option[Int] = {
		widget.dimensions(axis) match {
			case DimensionExpression.Constant(constValue) =>
				Some(constValue)
			case DimensionExpression.Proportional(proportion) =>
				Some((widget.parent.drawing.clientDim(axis) * proportion).round)
			case DimensionExpression.Relative(delta) =>
				Some(widget.parent.drawing.clientDim(axis) + delta)
			//			case DimensionExpression.ExpandToParent =>
			//				Some( widget.parent.drawing.clientDim(axis) - widget.drawing.relativePosition)
			case DimensionExpression.WrapContent =>
				val inPad = widget.drawing.interiorPadding
				val minV = Vec2i(0, 0)
				val maxV = Vec2i(0, 0)
				widget.children.foreach(w => {
					val rpos = w.drawing.relativePosition
					val edim = w.drawing.effectiveDimensions
					minV.x = minV.x.min(rpos.x)
					minV.y = minV.y.min(rpos.y)
					maxV.x = maxV.x.max(rpos.x + edim.x)
					maxV.y = maxV.y.max(rpos.y + edim.y)
				})
				Some(maxV(axis) - minV(axis) + inPad(axis) * 2)
			case _ => None
		}
	}

	def resolveRelativePosition(widget: Widget) = {
		val ret = new Vec3i(0, 0, 0)
		for (axis <- 0 until 3) {
			ret(axis) = (widget.position(axis) match {
				case PositionExpression.Constant(constValue, relativeTo) =>
					if (relativeTo == TopLeft || (axis == 0 && relativeTo == BottomLeft) || (axis == 1 && relativeTo == TopRight)) {
						constValue
					} else {
						widget.parent.drawing.clientDim(axis) - widget.drawing.effectiveDimensions(axis) - constValue
					}
				case PositionExpression.Proportional(p, relativeTo) =>
					val offsetExpr = widget.parent.drawing.clientDim(axis) * p
					if (relativeTo == TopLeft || (axis == 0 && relativeTo == BottomLeft) || (axis == 1 && relativeTo == TopRight)) {
						offsetExpr
					} else {
						widget.parent.drawing.clientDim(axis) - widget.drawing.effectiveDimensions(axis) - offsetExpr
					}
				case PositionExpression.Centered =>
					(widget.parent.drawing.clientDim(axis) - widget.drawing.effectiveDimensions(axis)) / 2
				case PositionExpression.Relative(relativeTo, offset, direction) =>
					val baseRelPos = if (relativeTo.parent == widget.parent) {
						relativeTo.drawing.relativePosition
					} else {
						relativeTo.drawing.absolutePosition - widget.parent.drawing.absolutePosition
					}

					direction match {
						case Cardinals.Right if axis == 0 =>
							baseRelPos.x + relativeTo.drawing.effectiveDimensions.x + offset
						case Cardinals.Left if axis == 0 =>
							baseRelPos.x - widget.drawing.effectiveDimensions.x - offset
						case Cardinals.Down if axis == 1 =>
							baseRelPos.y + relativeTo.drawing.effectiveDimensions.y + offset
						case Cardinals.Up if axis == 1 =>
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
						matchTo.drawing.absolutePosition(axis) - (widget.parent.drawing.absolutePosition(axis) + widget.parent.drawing.clientOffset(axis))
					}
				case PositionExpression.Flow =>
					0
				// do nothing there, this is considered unconstrained, will be filled in
				// more intelligently by the remaining layout engine...unless we deal with it
				// here, which we could
			}).round
		}
		ret
	}

	def updateResolvedWidgetVariables(w: Widget, resolved: mutable.Set[Widget]): Unit = {
		if (w.showing) {
			w.widget.unmarkModified()
			val DD = w[DrawingData]
			val (effDim, effClientArea) = resolveEffectiveDimensions(w)
			DD.effectiveDimensions = effDim
			DD.effectiveClientArea = effClientArea

			if (w.parent.notSentinel) {
				DD.relativePosition = resolveRelativePosition(w)
				DD.absolutePosition = w.parent.drawing.absolutePosition + Vec3i(w.parent.drawing.clientOffset, 0) + DD.relativePosition
			}

			resolved += w

			var toResolve = w.children
			while (toResolve.nonEmpty) {
				val picked = toResolve.head
				if (resolved(picked)) {
					toResolve = toResolve.tail
				} else {
					val requires = picked.x.dependsOn ::: picked.y.dependsOn ::: picked.width.dependsOn(picked) ::: picked.height.dependsOn(picked)
					val unfulfilled = requires.filterNot(resolved)
					unfulfilled match {
						case Nil =>
							updateResolvedWidgetVariables(picked, resolved)
							toResolve = toResolve.tail
						case _ =>
							toResolve = unfulfilled ::: toResolve
					}
				}
			}
		}
	}
}

object WindowingGraphicsComponent {

	case class WidgetWatchers(posWatcher: Watcher3[PositionExpression],
									  dimWatcher: Watcher2[DimensionExpression],
									  showingWatcher: Watcher[Boolean]) {
		var first = true

		def peekAnyChanged = posWatcher.peekChanged || dimWatcher.peekChanged || showingWatcher.peekChanged

		def anyChanged = posWatcher.hasChanged || dimWatcher.hasChanged || showingWatcher.hasChanged
	}

}