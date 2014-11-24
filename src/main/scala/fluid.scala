
class FluidSystem(dim: Vec2) {
  
  val densityField = new ScalarField.DensityField(dim)
  val velocityField = VectorField.dummy
  
  def update(dt: Double): Unit = {
    densityField.diffuse(dt)
    densityField.advect(dt, velocityField)
  }
  
}
