package fpinscala.monoids

import fpinscala.testing.{Gen, Prop}
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

  test("optionMonoid combines two options correctly") {
    Monoid.optionMonoid.op(Some(1), Some(2)) should be(Some(1))
  }

  test("endoMonoid combines two endofunctions correctly") {
    val f: Int => Int = _ + 1
    val g: Int => Int = _ * 2

    val fg = Monoid.endoMonoid.op(f, g)

    fg(3) should be(8)
  }

  test("monoidLaws should hold for String Monoid") {
    val chooseInt: Gen[Int] = Gen.choose(0, Int.MaxValue)
    val optionMonadLaws = Monoid.monoidLaws[Option[Int]](
      Monoid.optionMonoid[Int],
      chooseInt.map(Some(_))
    )
    // TODO - run returns unit but generates output to STDOUT
    Prop.run(optionMonadLaws)
  }

  test("concenate folds a list with a monoid correctly") {
    val data = List("foo", "bar", "baz")
    Monoid.concatenate(data, Monoid.stringMonoid) should be("foobarbaz")
  }

  test("foldMap folds a list correctly") {
    val data = List("foo", "bar", "baz")
    Monoid.foldMap(data, Monoid.stringMonoid)(identity) should be("foobarbaz")
  }
}
