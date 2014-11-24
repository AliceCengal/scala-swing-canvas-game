import math._

trait ForceField {
  def interact(p: Particle): Unit
  def update(): Unit = ()
}

object ForceField {

  def braking(maxV: Double): ForceField = new BrakingField(maxV)

  def gravitational(center: Vec2, k: Double): ForceField =
    new GravitationalField(center, k)

  def curlRightUniform(k: Double): ForceField = new CurlingFieldUniform(k)
  
  def curlLeftUniform(k: Double): ForceField = new CurlingFieldUniform(-k)

  def box(x: Double, y: Double): ForceField = new BoxField(Vec2.value(x, y))

  private final class BrakingField(val maxV: Double) extends ForceField {
    def interact(p: Particle) = {
      if (p.vel.magnitude > maxV) {
        p.applyForce(p.vel.negate * 0.25)
      }
    }
  }
  
  private final class GravitationalField(center: Vec2, k: Double) extends ForceField {
    def interact(p: Particle) = {
      val displacement = center - p.pos
      val force = k / math.pow(displacement.magnitude, 2)
      p.applyForce(displacement.normalized * force)
    }
  }
  
  private final class CurlingFieldUniform(k: Double) extends ForceField {
    def interact(p: Particle) = {
      p.applyForce(p.vel.rotate(Pi / 2) * k)
    }
  }
  
  private final class BoxField(dim: Vec2) extends ForceField {
    def interact(p: Particle) = {
      if (p.pos.x < 0.0) {
        p.pos.x = -p.pos.x
        p.vel.x = -p.vel.x
      }
      if (p.pos.x > dim.x) {
        p.pos.x -= 2*(p.pos.x - dim.x)
        p.vel.x = -p.vel.x
      }
      if (p.pos.y < 0.0) {
        p.pos.y = -p.pos.y
        p.vel.y = -p.vel.y
      }      
      if (p.pos.y > dim.y) {
        p.pos.y -= 2*(p.pos.y - dim.y)
        p.vel.y = -p.vel.y
      }
    }
  }
}