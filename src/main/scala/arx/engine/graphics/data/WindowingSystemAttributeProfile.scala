package arx.engine.graphics.data


/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/3/13
 * Time: 1:34 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.graphics.AttributeProfile
import org.lwjgl.opengl.GL11._

object WindowingSystemAttributeProfile extends AttributeProfile(
	"vertex" -> (2, GL_FLOAT) ::
		"texCoord" -> (2, GL_FLOAT) ::
		"color" -> (4, GL_FLOAT) ::
		"bounds" -> (4, GL_FLOAT) :: Nil
) {
	val V = attributesByName("vertex")
	val TC = attributesByName("texCoord")
	val C = attributesByName("color")
	val B = attributesByName("bounds")
}