package fpinscala.testing

import org.scalatest.FunSuite

class GenTest extends FunSuite {

  def genProp(b: Boolean) = new Prop {
    override def check = b
  }

  test("Combining two true props should generate a new prop with a check method that returns true") {
    val combinedProp = genProp(true) && genProp(true)
    combinedProp.check === true
  }

  test("Combining a true and false prop should generate a new prop with a check method that returns false") {
    val combinedProp = genProp(true) && genProp(false)
    combinedProp.check === false
  }
}
