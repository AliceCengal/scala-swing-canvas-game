
class VectorFieldTest extends org.scalatest.FunSuite {
  
  test("Field basis should be initialized to zero with the correct size") {
    val field = new FieldBasis(10, 15)
    assert(field(0, 0) == 0.0)
    assert(field(10, 0) == 0.0)
    assert(field(0, 15) == 0.0)
    assert(field(10, 15) == 0.0)
    assert(field(7, 10) == 0.0)
  }
  
  test("Field basis should be assignable") {
    val field = new FieldBasis(10, 15)
    field(5, 7) = 1.0
    assert(field(5, 7) == 1.0)
  }
  
  test("Calling swapBuffers in a newly created field after an assignment should clear the field") {
    val field = new FieldBasis(10, 15)
    field(5, 7) = 1.0
    field.swapBuffers()
    assert(field(5, 7) == 0.0)
    assert(field.prev(5, 7) == 1.0)
  }
  
  test("Field with zero boundary condition should have zero values at the borders after a call to setBoundary") {
    val field = new FieldBasis(10, 15) with BoundaryCondition.Zero
    for {
      i <- 0 to 11
      j <- 0 to 16
    } {
      field(i, j) = 5.0
    }
    
    field.setBoundary()
    
    for (j <- 0 to 16) {
      assert(field(0, j) == 0.0)
      assert(field(11, j) == 0.0)
    }
    
    for (i <- 1 to 10) {
      assert(field(i, 0) == 0.0)
      assert(field(i, 16) == 0.0)
      
      for (j <- 1 to 15) {
        assert(field(i, j) == 5.0)
      }
    }
  }
  
  test("Field with continuous boundary should have the borders equal to the point adjacent") {
    val field = new FieldBasis(10, 15) with BoundaryCondition.Continuous
    for {
      i <- 1 to 10
      j <- 1 to 15
    } {
      field(i, j) = 5.0
    }
    
    field.setBoundary()
    
    for (j <- 0 to 16) {
      assert(field(0, j) == 5.0)
      assert(field(11, j) == 5.0)
    }
    
    for (i <- 1 to 10) {
      assert(field(i, 0) == 5.0)
      assert(field(i, 16) == 5.0)
    }
  }
  
  test("Field with diffusivity should spread value around") {
    val field = new FieldBasis(10, 15) 
        with BoundaryCondition.Zero 
        with Diffusion { def diffusivity = 1.0 }
    
    field(5, 6) = 10.0
    
    field.diffuse(1.0)
    
    assert(field(5, 6) < 10.0)
    assert(field(7, 6) > 0.0 && field(7, 6) < 10.0)
    assert(field(3, 6) > 0.0 && field(3, 6) < 10.0)
    assert(field(5, 8) > 0.0 && field(5, 8) < 10.0)
    assert(field(5, 4) > 0.0 && field(5, 4) < 10.0)
  }
}
