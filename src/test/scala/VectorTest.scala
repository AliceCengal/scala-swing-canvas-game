import org.scalatest.FunSuite

class VectorTest extends FunSuite {
  
  test("A zero vector should have zero components") {
    val zeroVector = Vec2.zero
    assert(zeroVector.x == 0.0)
    assert(zeroVector.y == 0.0)
  }
  
  test("Adding a number to a vector should offset the vector by that amount") {
    val v = Vec2.value(0.0, 1.0)
    
    val added = v + 2.0
    assert(added.x == 2.0)
    assert(added.y == 3.0)
    
    val substracted = added - 2.0
    assert(substracted.x == v.x)
    assert(substracted.y == v.y)
  }
  
  test("Multiplying a vector by a number should scale the vector by that amount") {
    val v = Vec2.value(1.0, 2.0)
    
    val timesTwo = v * 2.0
    assert(timesTwo.x == 2.0)
    assert(timesTwo.y == 4.0)
    
    val dividedTwo = timesTwo / 2.0
    assert(dividedTwo.x == v.x)
    assert(dividedTwo.y == v.y)
  }
  
  test("Arythmetic operators other than the op-equal versions should not modify the original vector") {
    val v = Vec2.value(5.0, 2.0)
    v + 2.0
    v - 6.0
    v * 8.0
    v / 7.0
    v + v
    v - v
    v.negate
    assert(v.x == 5.0)
    assert(v.y == 2.0)
  }
  
  test("Calling magnitude() should return the magnitude of the vector") {
    val v = Vec2.value(4.0, 3.0)
    assert(v.magnitude == 5.0)
  }
  
  test("Calling rotate() should return a rotation of the vector by the given radians as a new vector") {
    val v = Vec2.value(4.0, 3.0)
    val rotated = v.rotate(math.Pi / 2)
    assert(rotated.x > -3.001 && rotated.x < -2.999)
    assert(rotated.y > 3.999 && rotated.y < 4.001)
  }
}