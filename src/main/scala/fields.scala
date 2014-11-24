
trait ScalarField {
  def value(x: Int, y: Int): Double
}

object ScalarField {

  def density(dim: Vec2) = new DensityField(dim)
  
  final class DensityField(dim: Vec2) extends ScalarField {
    
    val (xDim, yDim) = (dim.x.toInt, dim.y.toInt)
    
    private var basisPrev = Array.fill((xDim + 2) * (yDim + 2))(0.0)
    private var basis = Array.fill((xDim + 2) * (yDim + 2))(0.0)
    
    val diffusionConstant = 1.0
    val boundarySet = 0.0
    
    @inline private def ix(x: Int, y: Int): Int = x + y*(xDim + 2)
    
    def value(x: Int, y: Int) = basis(ix(x, y))
    
    def diffuse(dt: Double): Unit = {
      
      // Prepare the array buffers for processing
      swapBuffers()
      zeroArray(basis)
      
      val a = diffusionConstant * dim.x * dim.y * dt
      
      // Do the numerical diffusion iteration twenty times. #magicnumber
      // Gauss-Seidel relaxation
      var iter = 20
      while (iter >= 0) {
        
        var xx = xDim
        while (xx > 0) {
          
          var yy = yDim
          while (yy > 0) {
            
            // Do diffusion processing
            basis(ix(xx, yy)) =
              (basisPrev(ix(xx, yy)) +
                a * (basis(ix(xx + 1, yy)) + basis(ix(xx - 1, yy)) +
                  basis(ix(xx, yy + 1)) + basis(ix(xx, yy - 1)))) /
              //--------------------------------------------------
                (1 + 4 * a)
            
            yy -= 1
          }
          xx -= 1
        }
        
        setBoundaries(basis, 0.0)
      }
    }
    
    def advect(dt: Double, velField: VectorField): Unit = {
      
      // Prepare the array buffers for processing
      swapBuffers()
      val dt0 = dt * xDim
      
      // Iteration
      var xx = xDim
      while (xx > 0) {
        
        var yy = yDim
        while (yy > 0) {
          
          val disp = Vec2.value(xx, yy) - velField.value(xx, yy)*dt0
          
          if (disp.x < 0.5) disp.x = 0.5
          if (disp.x > (xDim + 0.5)) disp.x = (xDim + 0.5)
          val xx0 = disp.x.toInt
          val xx1 = xx0 + 1
          
          if (disp.y < 0.5) disp.y = 0.5
          if (disp.y > (yDim + 0.5)) disp.y = (yDim + 0.5)
          val yy0 = disp.y.toInt
          val yy1 = yy0 + 1
          
          val s1 = disp.x - xx0
          val s0 = 1 - s1
          
          val t1 = disp.y - yy0
          val t0 = 1 - t1
          
          basis(ix(xx, yy)) =
             s0 * (t0*basisPrev(ix(xx0,yy0)) + t1*basisPrev(ix(xx0,yy1))) + 
             s1 * (t0*basisPrev(ix(xx1,yy0)) + t1*basisPrev(ix(xx1,yy1)));
          
          yy -= 1
        }
        xx -= 1
      }
      setBoundaries(basis, 0.0)
    }
    
    private def setBoundaries(array: Array[Double], value: Double): Unit = {
      
    }
    
    private def swapBuffers(): Unit = {
      val tmp = basisPrev
      basisPrev = basis
      basis = tmp
    }
  }
  
  private def zeroArray(array: Array[Double]): Unit = {
    var n = array.length - 1
    while (n >= 0) {
      array(n) = 0.0
      n -= 1
    }
  }
  
}

trait VectorField {
  def value(x: Int, y: Int): Vec2
}

object VectorField {
  def dummy = new VectorField {
    def value(x: Int, y: Int) = Vec2.zero
  }
}

class FieldBasis(val xDim: Int, val yDim: Int) {
  private var basisPrev = Array.fill(xDim + 2, yDim + 2)(0.0)
  private var basis = Array.fill(xDim + 2, yDim + 2)(0.0)
  
  @inline def apply(x: Int, y: Int): Double = basis(x)(y)
  
  def update(x: Int, y: Int, value: Double): Unit = basis(x)(y) = value
  
  def swapBuffers(): Unit = {
    val tmp = basisPrev
    basisPrev = basis
    basis = tmp
  }
  
}

trait Diffusion {
  self: FieldBasis with BoundaryCondition =>
  
  def diffusivity: Double
  
  def diffuse(dt: Double): Unit = {
    
  }
  
}

trait Advection {
  self: FieldBasis with BoundaryCondition =>
  
  def advection(dt: Double, carrier: VectorField): Unit = {
    
  }
  
}

trait BoundaryCondition {
  
  def setBoundary(): Unit
  
}

object BoundaryCondition {
  
  trait Zero extends BoundaryCondition {
    self: FieldBasis =>
      
    def setBoundary(): Unit = {
      { // Top and bottom edges
        var i = self.xDim + 1
        while (i >= 0) {
          self(i, 0) = 0.0
          self(i, yDim + 2) = 0.0
          i -= 1
        }
      }
      { // Left and right edges
        var i = self.yDim
        while (i > 0) {
          self(0, i) = 0.0
          self(xDim + 2, i) = 0.0
          i -= 1
        }
      }
    }
  }
  
  trait Continuous extends BoundaryCondition {
    self: FieldBasis =>
      
    def setBoundary(): Unit = {
      { // Top and bottom edges
        var i = self.xDim
        while (i > 0) {
          self(i, 0) = self(i, 1)
          self(i, yDim + 2) = self(i, yDim + 1)
        }
      }
      { // Left and right edges
        var i = self.yDim
        while (i > 0) {
          
        }
      }
    }
  }
}