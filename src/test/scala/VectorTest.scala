import org.scalatest.FunSuite

class VectorTest extends FunSuite {
  
  test("A zero vector should have zero components") {
    val zeroVector = Vec2.zero
    assert(zeroVector.x == 0.0)
    assert(zeroVector.y == 0.0)
  }
  
}