package arx.engine

import arx.core.vec.ReadVec2f
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.event.{CharEnteredEvent, KeyModifiers, KeyPressEvent, KeyReleaseEvent, KeyboardMirror, Mouse, MouseButton, MouseDragEvent, MouseMoveEvent, MousePressEvent, MouseReleaseEvent, ScrollEvent}
import arx.engine.event.{Event, EventBus, TEventUser}
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.world.{Universe, World}
import arx.resource.ResourceManager
import org.lwjgl.glfw.GLFW

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

}

class Engine extends EngineCore with TEventUser {

	var gameEngine : Option[GameEngine] = None
	var graphicsEngine : Option[GraphicsEngine] = None
	var controlEngine : Option[ControlEngine] = None

	var engines = List[EnginePiece[_,_]]()

	/**
	 * Load the listed scenario, using the existing state of the given universe. Will be a blank universe when
	 * starting a new game, could be a deserialized universe when loading saved state
	 */
	def loadScenario(scenario : Scenario, universe : Universe): Unit = {
		val gameWorld = scenario.gameWorld(universe)
		val displayWorld = scenario.displayWorld(universe)

		val gameEngine = new GameEngine(scenario.realtime(universe), universe, gameWorld)
		val graphicsEngine = new GraphicsEngine(gameEngine, universe, displayWorld)
		val controlEngine = new ControlEngine(gameEngine, graphicsEngine, universe, displayWorld)

		scenario.registerGameComponents(gameEngine, universe)
		scenario.registerGraphicsComponents(graphicsEngine, universe)
		scenario.registerControlComponents(controlEngine, universe)

		def componentInitializer(any : Any) = any match {
			case gameComponent : GameComponent => gameEngine.initializeComponent(gameComponent)
			case graphicsComponent : GraphicsComponent => graphicsEngine.initializeComponent(graphicsComponent)
			case controlComponent : ControlComponent => controlEngine.initializeComponent(controlComponent)
			case _ => // do nothing for non-components
		}

		var context : List[Any] = Nil
		context = gameEngine.resolveComponents(context, componentInitializer)
		context = graphicsEngine.resolveComponents(context, componentInitializer)
		context = controlEngine.resolveComponents(context, componentInitializer)

		gameEngine.initialize(scenario.serialGameEngine(universe))
		graphicsEngine.initialize(scenario.serialGraphicsEngine(universe))
		controlEngine.initialize(scenario.serialControlEngine(universe))

		engines = List(gameEngine, graphicsEngine, controlEngine)
		this.gameEngine = Some(gameEngine)
		this.graphicsEngine = Some(graphicsEngine)
		this.controlEngine = Some(controlEngine)
	}

	override def update(deltaSeconds: Float): Unit = {
		for (ge <- gameEngine) { ge.update(deltaSeconds) }
		for (ge <- graphicsEngine) { ge.update(deltaSeconds) }
		for (ce <- controlEngine) { ce.update(deltaSeconds) }
	}

	override def draw(): Unit = {
		for (ge <- graphicsEngine) { ge.draw() }
	}

	onEventFallback {
		case KeyPressEvent(key,_,_) if key == GLFW.GLFW_KEY_F2 =>
			ResourceManager.refreshImages()
			ResourceManager.reloadShaders()
		case e : Event => for (ce <- controlEngine) { ce.eventBus.fireEvent(e) }
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

	def main (args : Array[String]): Unit = {
		scalaMain(args)
	}
}
