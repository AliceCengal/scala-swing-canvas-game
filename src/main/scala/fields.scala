import math._

trait Field {
  def interact(p: Particle): Unit
  def update(): Unit = ()
}

object Field {

  def braking(maxV: Double): Field = new BrakingField(maxV)

  def gravitational(center: Vec2, k: Double): Field =
    new GravitationalField(center, k)

  def curlRightUniform(k: Double): Field = new CurlingFieldUniform(k)
  
  def curlLeftUniform(k: Double): Field = new CurlingFieldUniform(-k)

  def box(x: Double, y: Double): Field = new BoxField(Vec2.value(x, y))

  private final class BrakingField(val maxV: Double) extends Field {
    def interact(p: Particle) = {
      if (p.vel.magnitude > maxV) {
        p.applyForce(p.vel.negate * 0.25)
      }
    }
  }
  
  private final class GravitationalField(center: Vec2, k: Double) extends Field {
    def interact(p: Particle) = {
      val displacement = center - p.pos
      val force = k / math.pow(displacement.magnitude, 2)
      p.applyForce(displacement.normalized * force)
    }
  }
  
  private final class CurlingFieldUniform(k: Double) extends Field {
    def interact(p: Particle) = {
      p.applyForce(p.vel.rotate(Pi / 2) * k)
    }
  }
  
  private final class BoxField(dim: Vec2) extends Field {
    def interact(p: Particle) = {
      if (p.pos.x < 0) {
        p.pos.x = -p.pos.x
        p.vel.x = -p.vel.x
      }
      if (p.pos.x > dim.x) {
        p.pos.x = dim.x
        p.vel.x = -p.vel.x
      }
      if (p.pos.y < 0) {
        p.pos.y = -p.pos.y
        p.vel.y = -p.vel.y
      }      
      if (p.pos.y > dim.y) {
        p.pos.y = dim.y
        p.vel.y = -p.vel.y
      }
    }
  }
}