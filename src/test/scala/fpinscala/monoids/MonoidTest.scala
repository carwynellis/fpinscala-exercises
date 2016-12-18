package fpinscala.monoids

import org.scalatest.{FunSuite, Matchers}

class MonoidTest extends FunSuite with Matchers {

  test("stringMonoid combines two strings correctly") {
    Monoid.stringMonoid.op("foo", "bar") should be("foobar")
  }

  test("listMonoid combines two lists correctly") {
    Monoid.listMonoid.op(List(1,2), List(3,4)) should be(List(1,2,3,4))
  }

}
