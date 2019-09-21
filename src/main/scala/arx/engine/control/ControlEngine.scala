package arx.engine.control

import arx.engine.EnginePiece
import arx.engine.control.components.ControlComponent
import arx.engine.game.GameEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.world.{Universe, World}

class ControlEngine(val gameEngine : GameEngine, val graphicsEngine : GraphicsEngine, universe : Universe, val displayWorld : World) extends EnginePiece[ControlEngine, ControlComponent](universe) {

}
