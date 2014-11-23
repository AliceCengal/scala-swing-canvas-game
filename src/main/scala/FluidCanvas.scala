
import scala.swing._
//import event._
import java.awt.{ Color, Graphics2D }
import java.awt.event.{ActionListener, ActionEvent}
import scala.util.Random

object FluidCanvas extends SimpleSwingApplication with ActionListener {

  val prototype = new Particle

  // The universe of particles to be simulated
  val universe = new Universe {
    val particles =
      (0 until 100).map { _ =>
          prototype
            .randomlyIn(1600, 1000)
            .randomVelocity(100, 100) }
        .toVector
    
    val forceFields = Vector(
      ForceField.curlRightUniform(0.25), 
      ForceField.box(1600, 1000))
  }

  // The main canvas on which the universe is drawn on
  val canvas = new Canvas(universe)
  
  // The time of the previous tick, used to track FPS
  var time = System.currentTimeMillis
  
  // Define the main user interface
  def top = new MainFrame {
    title = "A Sample Scala Swing GUI"
    contents = canvas
    size = new Dimension(1600, 1000)
    centerOnScreen()
  }
  
  def actionPerformed(e: ActionEvent): Unit = {
    val dt = System.currentTimeMillis - time
    canvas.time = dt
    universe.update(dt * 0.001)
    time = System.currentTimeMillis
    canvas.repaint()
  }
  
  val timer = new javax.swing.Timer(1000/60, this)
  timer.start()
}

class Canvas(val universe: Universe) extends Panel {

  var time = 0l

  override def paintComponent(g: Graphics2D) {
    
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
    // Draw background here
    g.setColor(Color.blue)
    
    g.drawString(time.toString, 10, 10)
    
    // Draw things that change on top of background
    for (p <- universe.particles) {
      g.fillOval(p.pos.x.toInt, p.pos.y.toInt, 10, 10)
    }
  }

}