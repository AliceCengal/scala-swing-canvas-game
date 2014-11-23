
import scala.swing._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random

object FluidCanvas extends SimpleSwingApplication {

  val prototype = new Particle

  val universe = new Universe {
    val particles =
      (0 until 100).map { _ =>
          prototype
            .randomlyIn(1600, 1000)
            .randomVelocity(100, 100) }
        .toVector
    
    val fields = Vector(
      Field.curlRightUniform(0.25), 
      Field.box(1600, 1000))
  }

  val canvas = new Canvas(universe)
  
  var time = System.currentTimeMillis
  
  def top = new MainFrame {
    title = "A Sample Scala Swing GUI"
    contents = canvas
    size = new Dimension(1600, 1000)
    centerOnScreen()
  }
  
  def update(): Unit = {
    val dt = System.currentTimeMillis - time
    canvas.time = dt
    universe.update(dt * 0.001)
    time = System.currentTimeMillis
    canvas.repaint()
    Swing.onEDT(update)
  }
  
  Swing.onEDT(update)
}

class Canvas(val universe: Universe) extends Panel {

  var time = 0.0

  override def paintComponent(g: Graphics2D) {
    
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
    // Draw background here
    g.setColor(Color.blue)
    
    //g.drawString(time.toString, 10, 10)
    
    // Draw things that change on top of background
    for (p <- universe.particles) {
      g.fillOval(p.pos.x.toInt, p.pos.y.toInt, 10, 10)
    }
  }

}