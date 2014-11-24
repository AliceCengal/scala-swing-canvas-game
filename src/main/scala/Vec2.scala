
import math._

object Vec2 {
  
  def zero = new Vec2(0, 0)
  
  def value(x: Double, y: Double) = new Vec2(x, y)
  
}

// A class that represents an R2 vector. It is mutable, to reduce the GC load
// in tight loops.
final class Vec2(var x: Double, var y: Double) {
  
  def +(offset: Double) =
    new Vec2(x + offset, y + offset)
  
  def -(offset: Double) =
    new Vec2(x - offset, y - offset)
  
  def *(scale: Double) =
    new Vec2(x * scale, y * scale)
  
  def /(scale: Double) =
    new Vec2(x / scale, y / scale)
  
  def +(rhs: Vec2) =
    new Vec2(x + rhs.x, y + rhs.y)
  
  def -(rhs: Vec2) =
    new Vec2(x - rhs.x, y - rhs.y)
  
  def +=(offset: Double) = {
    this.x += offset
    this.y += offset
    this
  }
  
  def -=(offset: Double) = {
    this.x -= offset
    this.y -= offset
    this
  }
  
  def *=(scale: Double) = {
    this.x *= scale
    this.y *= scale
    this
  }
  
  def /=(scale: Double) = {
    this.x /= scale
    this.y /= scale
    this
  }
  
  def +=(rhs: Vec2) = {
    this.x += rhs.x
    this.y += rhs.y
    this
  }
  
  def -=(rhs: Vec2) = {
    this.x -= rhs.x
    this.y -= rhs.y
    this
  }
  
  override def clone = new Vec2(x, y)
  
  def negate = new Vec2(-x, -y)
  
  def magnitude = sqrt( x*x + y*y )
  
  def normalized = this / magnitude
  
  def dot(rhs: Vec2) =
    (this.x * rhs.x) + (this.y * rhs.y)
  
  def rotate(rad: Double) =
    new Vec2(
      x * cos(rad) - y * sin(rad),
      x * sin(rad) + y * cos(rad))
  
  def ==(rhs: Vec2): Boolean = this.x == rhs.x && this.y == rhs.y
  
}