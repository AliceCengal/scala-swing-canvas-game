import math._

trait Transform {
  def apply(from: Vec2): Vec2
}

object Transform {
  
  def transpose: Transform = Transpose
  
  def rotate(radians: Double): Transform = 
    new Rotate(radians)
  
  def rotateAngle(deg: Double): Transform = 
    new Rotate(deg / 180.0 * Pi)
  
  def scaling(scale: Double): Transform =
    new Scaling(scale)
  
  private object Transpose extends Transform {
    override def apply(from: Vec2) =
      Vec2.value(from.y, from.x)
  }

  private final class Rotate(rad: Double) extends Transform {
    override def apply(from: Vec2) =
      from.rotate(rad)
  }
  
  private final class Scaling(scale: Double) extends Transform {
    override def apply(from: Vec2) = from * scale
  }
}

