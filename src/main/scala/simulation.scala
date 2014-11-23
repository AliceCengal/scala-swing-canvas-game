
final class Particle(
  val pos: Vec2 = Vec2.zero,
  val vel: Vec2 = Vec2.zero,
  val mass: Double = 1.0) extends ParticlePrototype
{
  private val appliedForce = Vec2.zero
  
  def update(dt: Double) {
    vel += (appliedForce / mass * dt)
    pos += (vel * dt)
    appliedForce.x = 0
    appliedForce.y = 0
  }
  
  def applyForce(f: Vec2) {
    appliedForce += f
  }
}

trait ParticlePrototype {
  self: Particle =>
  
  import scala.util.Random
  
  override def clone = new Particle(pos.clone, vel.clone, mass)
  
  def randomlyIn(limitX: Double, limitY: Double) =
    new Particle(
      pos = Vec2.value(Random.nextDouble * limitX, Random.nextDouble * limitY),
      vel = vel,
      mass = mass)
  
  def randomVelocity(limitX: Double, limitY: Double) =
    new Particle(
      pos = pos,
      vel = Vec2.value(
        (Random.nextDouble - 0.5) * limitX,
        (Random.nextDouble - 0.5) * limitY),
      mass = mass)
}

object Particle {
  def withMass(m: Double) = new Particle(mass = m)
}

trait Universe {
  
  def particles: Vector[Particle]
  
  def fields: Vector[Field]
  
  def update(dt: Double) {
    
    
    var np = 0
    while (np < particles.length) {
      val p = particles(np)
      var nf = 0
      while (nf < fields.length) {
        fields(nf).interact(p)
        nf += 1
      }
      p.update(dt)
      np += 1
    }
    
    /*
    particles.foreach { p =>
      fields.foreach(_.interact(p))
      p.update(dt)
    }
    */
  }
}