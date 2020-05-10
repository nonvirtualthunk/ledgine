package arx.engine

import java.util.concurrent.{ExecutorCompletionService, Executors, TimeUnit}

import arx.Prelude
import arx.application.Noto
import arx.core.async.{NamedThreadFactory, OnExitRegistry}
import arx.core.metrics.Metrics
import arx.core.vec.{ReadVec2f, ReadVec2i, Vec2i}
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.event.{CharEnteredEvent, ControlEvent, KeyModifiers, KeyPressEvent, KeyReleaseEvent, KeyboardMirror, Mouse, MouseButton, MouseDragEvent, MouseMoveEvent, MousePressEvent, MouseReleaseEvent, ScrollEvent}
import arx.engine.event.{Event, EventBus, GameEvent, TEventUser, WorldCreatedEvent}
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.world.{Universe, World}
import arx.graphics.GL
import arx.resource.ResourceManager
import org.lwjgl.glfw.{GLFW, GLFWVidMode}
import org.lwjgl.opengl.{EXTFramebufferSRGB, GL30}

trait Scenario {
	def gameWorld(universe : Universe) : World

	def displayWorld(universe : Universe) : World

	def realtime(universe : Universe) : Boolean

	def registerGameComponents(gameEngine : GameEngine, universe : Universe)

	def registerGraphicsComponents(graphicsEngine: GraphicsEngine, universe : Universe)

	def registerControlComponents(controlEngine : ControlEngine, universe : Universe)

	def serialGameEngine(universe : Universe) : Boolean = false
	def serialGraphicsEngine(universe : Universe) : Boolean = false
	def serialControlEngine(universe : Universe) : Boolean = false

	/**
	 * Called after game engine initialized
	 */
	def setupInitialGameState(world : World)
}

class Engine extends EngineCore with TEventUser {

	var gameEngine : Option[GameEngine] = None
	var graphicsEngine : Option[GraphicsEngine] = None
	var controlEngine : Option[ControlEngine] = None

	var engines = List[EnginePiece[_,_,_]]()


	override def onInit(): Unit = {
	}

	/**
	 * Load the listed scenario, using the existing state of the given universe. Will be a blank universe when
	 * starting a new game, could be a deserialized universe when loading saved state
	 */
	def loadScenario(scenario : Scenario, universe : Universe): Unit = {
//		val windowSize = scenario.desiredWindowSize(universe)
//		GLFW.glfwSetWindowSize(window, windowSize.x, windowSize.y)
//		val monitor = GLFW.glfwGetWindowMonitor(window) match {
//			case 0 => GLFW.glfwGetPrimaryMonitor()
//			case other => other
//		}
//		val xa = Array.ofDim[Float](1)
//		val ya = Array.ofDim[Float](1)
//
//		val xia = Array.ofDim[Int](1)
//		val yia = Array.ofDim[Int](1)
//		val wa = Array.ofDim[Int](1)
//		val ha = Array.ofDim[Int](1)
//		GLFW.glfwGetMonitorContentScale(monitor, xa, ya)
//		GLFW.glfwGetMonitorWorkarea(monitor, xia, yia, wa, ha)
//		GLFW.glfwSetWindowPos(window, xia(0) + (wa(0) - windowSize.x) / 2, yia(0) + (ha(0) - windowSize.y) / 2)

		val scenarioName = scenario.getClass.getSimpleName
		Metrics.checkpoint(s"scenario $scenarioName load started")

		val gameWorld = scenario.gameWorld(universe)
		gameWorld.addEvent(new WorldCreatedEvent)
		val displayWorld = scenario.displayWorld(universe)
		displayWorld.addEvent(new WorldCreatedEvent)
		Metrics.checkpoint(s"scenario $scenarioName world objects created")

		val gameEngine = new GameEngine(scenario.realtime(universe), universe, gameWorld)
		val graphicsEngine = new GraphicsEngine(gameEngine, universe, displayWorld)
		val controlEngine = new ControlEngine(gameEngine, graphicsEngine, universe, displayWorld)

		Metrics.checkpoint(s"scenario $scenarioName engines created")

		scenario.registerGameComponents(gameEngine, universe)
		scenario.registerGraphicsComponents(graphicsEngine, universe)
		scenario.registerControlComponents(controlEngine, universe)

		Metrics.checkpoint(s"scenario $scenarioName components registered")

		def componentInitializer(any : Any) = any match {
			case gameComponent : GameComponent => gameEngine.initializeComponent(gameComponent)
			case graphicsComponent : GraphicsComponent => graphicsEngine.initializeComponent(graphicsComponent)
			case controlComponent : ControlComponent => controlEngine.initializeComponent(controlComponent)
			case _ => // do nothing for non-components
		}

		var context : List[Any] = Nil
		context = gameEngine.resolveComponents(context, componentInitializer)
		scenario.setupInitialGameState(gameWorld)
		context = graphicsEngine.resolveComponents(context, componentInitializer)
		context = controlEngine.resolveComponents(context, componentInitializer)

		Metrics.checkpoint(s"scenario $scenarioName components resolved")

		gameEngine.initialize(scenario.serialGameEngine(universe))
		Metrics.checkpoint(s"scenario $scenarioName game engine initialized")
		graphicsEngine.initialize(scenario.serialGraphicsEngine(universe))
		Metrics.checkpoint(s"scenario $scenarioName graphics engine initialized")
		controlEngine.initialize(scenario.serialControlEngine(universe))
		Metrics.checkpoint(s"scenario $scenarioName control engine initialized")

		engines = List(gameEngine, graphicsEngine, controlEngine)
		this.gameEngine = Some(gameEngine)
		this.graphicsEngine = Some(graphicsEngine)
		this.controlEngine = Some(controlEngine)

		Metrics.checkpoint(s"scenario $scenarioName load finished")
	}

	def assignWorlds(event : Event) : Event = event match {
		case ge : GameEvent =>
			for (engine <- this.gameEngine) {
				ge.world = engine.world
			}
			ge
		case ce : ControlEvent =>
			for (engine <- this.controlEngine) {
				ce.withWorlds(engine.gameEngine.world, engine.displayWorld)
			}
			ce
		case e => e
	}

	val graphicsThread = Executors.newSingleThreadExecutor(NamedThreadFactory("graphics-thread"))
	val controlThread = Executors.newSingleThreadExecutor(NamedThreadFactory("control-thread"))
	val gameThread = Executors.newSingleThreadExecutor(NamedThreadFactory("game-thread"))

	OnExitRegistry.register(() => {
		for (pool <- List(graphicsThread, controlThread, gameThread)){
			pool.shutdown()
			if (!pool.awaitTermination(3, TimeUnit.SECONDS)) {
				pool.shutdownNow()
			}
		}
	})

	override def update(deltaSeconds: Float): Unit = {
		val tasks = List(
			gameThread.submit((() => for (ge <- gameEngine) { ge.update(deltaSeconds) }) : Runnable),
			graphicsThread.submit((() => for (ge <- graphicsEngine) { ge.update(deltaSeconds) }) : Runnable),
			controlThread.submit((() => for (ge <- controlEngine) { ge.update(deltaSeconds) }) : Runnable)
		)

		tasks.foreach(_.get())
//		// note, stack overflow
//		for (future <- tasks) {
//      future.get()
//    }

	}

	override def draw(): Unit = {
		for (ge <- graphicsEngine) { ge.draw() }
	}

	onEventFallback {
		case KeyPressEvent(key,_,_) if key == GLFW.GLFW_KEY_F2 =>
			ResourceManager.refreshImages()
			ResourceManager.reloadShaders()
		case e : ControlEvent => for (ce <- controlEngine) { ce.eventBus.fireEvent(e) }
	}

	// Transform callbacks into event objects
	override def keyCallback(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
		val keyMods = KeyModifiers.fromGLFW(mods)
		val event = action match {
			case GLFW.GLFW_PRESS => KeyPressEvent(key, keyMods)
			case GLFW.GLFW_RELEASE => KeyReleaseEvent(key, keyMods)
			case GLFW.GLFW_REPEAT => KeyPressEvent(key, keyMods, isRepeat = true)
		}
		this.handleEvent(event)
	}

	override def charCallback(str: String): Unit = this.handleEvent(CharEnteredEvent(str))

	override def mouseButtonCallback(button: MouseButton, action: Int, mods: Int): Unit = {
		val keyMods = KeyModifiers.fromGLFW(mods)
		val event = action match {
			case GLFW.GLFW_PRESS => MousePressEvent(button, Mouse.currentPosition, keyMods)
			case GLFW.GLFW_RELEASE => MouseReleaseEvent(button, Mouse.currentPosition, keyMods)
		}
		this.handleEvent(event)
	}

	override def mousePosCallback(x: Float, y: Float): Unit = {
		val keyMods = KeyboardMirror.activeModifiers
		val event = if (Mouse.buttonDown.exists(t => t._2)) {
			val buttons = Mouse.buttonDown.filter(t => t._2).keys.toSet
			MouseDragEvent(Mouse.currentPosition, Mouse.currentPosition - Mouse.previousPosition, buttons, keyMods)
		} else {
			MouseMoveEvent(Mouse.currentPosition, Mouse.currentPosition - Mouse.previousPosition, keyMods)
		}
		this.handleEvent(event)
	}

	override def scrollCallback(dx: Float, dy: Float): Unit = {
		val event = ScrollEvent(ReadVec2f(dx,dy), KeyboardMirror.activeModifiers)
		this.handleEvent(event)
	}


	override def initialWindowPos(vidmode: GLFWVidMode): ReadVec2i = {
		val default = super.initialWindowPos(vidmode)
		Vec2i(default.x, 0)
	}

	/* Make sure every event has its worlds assigned, if relevant, before handling */
	override def handleEvent(event: Event): Boolean = super.handleEvent(assignWorlds(event))

	def main (args : Array[String]): Unit = {
		scalaMain(args)
	}
}
