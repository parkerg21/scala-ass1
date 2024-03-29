package recfun


import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  test("pascal: col=0,row=0"){
    assert(pascal(0,0) === 1)
  }
  test("pascal throws an exception if its argument is negative") {
    intercept[IllegalArgumentException] {
      pascal(-1,0)
    }
  }
  test("pascal upper boundary test of large inputs"){
    intercept[IllegalArgumentException] {
      pascal(1000,1000)  
    }
  }
}
