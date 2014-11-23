
trait ScalarField {
  def value(pos: Vec2): Double
}

object ScalarField {
  
  def density: ScalarField = new DensityField
  
  private final class DensityField extends ScalarField {
    def value(pos: Vec2) = 1.0
  }
  
}