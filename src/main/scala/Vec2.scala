
import math._

object Vec2 {
  
  def zero = new Vec2(0, 0)
  
  def value(x: Double, y: Double) = new Vec2(x, y)
  
}

class Vec2(var x: Double, var y: Double) {
  
  final def +(offset: Double) =
    new Vec2(x + offset, y + offset)
  
  final def -(offset: Double) =
    new Vec2(x - offset, y - offset)
  
  final def *(scale: Double) =
    new Vec2(x * scale, y * scale)
  
  final def /(scale: Double) =
    new Vec2(x / scale, y / scale)
  
  final def +(rhs: Vec2) =
    new Vec2(x + rhs.x, y + rhs.y)
  
  final def -(rhs: Vec2) =
    new Vec2(x - rhs.x, y - rhs.y)
  
  final def +=(offset: Double) = {
    this.x += offset
    this.y += offset
    this
  }
  
  final def -=(offset: Double) = {
    this.x -= offset
    this.y -= offset
    this
  }
  
  final def *=(scale: Double) = {
    this.x *= scale
    this.y *= scale
    this
  }
  
  final def /=(scale: Double) = {
    this.x /= scale
    this.y /= scale
    this
  }
  
  final def +=(rhs: Vec2) = {
    this.x += rhs.x
    this.y += rhs.y
    this
  }
  
  final def -=(rhs: Vec2) = {
    this.x -= rhs.x
    this.y -= rhs.y
    this
  }
  
  final override def clone = new Vec2(x, y)
  
  final def negate = new Vec2(-x, -y)
  
  final def magnitude = sqrt( x*x + y*y )
  
  final def normalized = this / magnitude
  
  final def dot(rhs: Vec2) =
    (this.x * rhs.x) + (this.y * rhs.y)
  
  final def rotate(rad: Double) =
    new Vec2(
      x * cos(rad) - y * sin(rad),
      x * sin(rad) + y * cos(rad))
  
}