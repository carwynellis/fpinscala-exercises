package fpinscala.monoids

import org.scalatest.{FunSuite, Matchers}

class MonoidTest extends FunSuite with Matchers {

  test("stringMonoid combines two strings correctly") {
    Monoid.stringMonoid.op("foo", "bar") should be("foobar")
  }

  test("listMonoid combines two lists correctly") {
    Monoid.listMonoid.op(List(1,2), List(3,4)) should be(List(1,2,3,4))
  }

  test("intAddition monoid adds two ints correctly") {
    Monoid.intAddition.op(1, 2) should be(3)
  }

  test("intMultiplication monoid adds two ints correctly") {
    Monoid.intMultiplication.op(1, 2) should be(2)
  }

  test("booleanOr monoid combines two booleans correctly") {
    Monoid.booleanOr.op(true, false) should be(true)
  }

  test("booleanAnd monoid combines two booleans correctly") {
    Monoid.booleanAnd.op(true, false) should be(false)
  }

}
