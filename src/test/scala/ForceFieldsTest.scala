import org.scalatest.FunSuite

class ForceFieldsTest extends FunSuite {
  
  test("Braking field should reduce particle velocity if it exceeds the max velocity") {
    val brakingField = ForceField.braking(50)
    val particle = new Particle(vel = Vec2.value(60, 70))
    val originalSpeed = particle.vel.magnitude
    
    brakingField.interact(particle)
    particle.update(1.0)
    assert(particle.vel.magnitude < originalSpeed)
  }
  
  test("Braking field should not affect the particle if it is below the max velocity") {
    val brakingField = ForceField.braking(50)
    val particle = new Particle(vel = Vec2.value(30, 30))
    val originalSpeed = particle.vel.magnitude
    
    brakingField.interact(particle)
    particle.update(1.0)
    assert(particle.vel.magnitude == originalSpeed)
  }
  
  test("Box field should not affect the particle if it is in the box") {
    val boxField = ForceField.box(10.0, 10.0)
    val particle = new Particle(pos = Vec2.value(5.0, 5.0))
    
    boxField.interact(particle)
    particle.update(1.0)
    assert(particle.pos.x == 5.0 && particle.pos.y == 5.0)
  }
  
  test("Box field should keep the particle in the box") {
    val boxField = ForceField.box(10.0, 10.0)
    val particle = new Particle(pos = Vec2.value(5, 5))
    
    particle.pos.x = 11.0
    boxField.interact(particle)
    particle.update(1.0)
    assert(particle.pos.x > 0.0 && particle.pos.x < 10.0)
    
    particle.pos.x = -1.0
    boxField.interact(particle)
    particle.update(1.0)
    assert(particle.pos.x > 0.0 && particle.pos.x < 10.0)
  }
}