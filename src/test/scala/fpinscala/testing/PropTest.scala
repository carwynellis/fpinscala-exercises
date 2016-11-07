package fpinscala.testing

import fpinscala.state.RNG
import org.scalatest.{FunSuite, Matchers}

class PropTest extends FunSuite with Matchers {

  test("the && operator should return passed for two properties that pass") {
    val gen = Gen.choose(1, 10)

    val lessThanTen = Prop.forAll(gen)(a => a < 10)
    val greaterThanZero = Prop.forAll(gen)(a => a > 0)

    val combined = lessThanTen && greaterThanZero

    val result = combined.run(100, RNG.Simple(12345))

    result.isFalsified should be(false)
  }

  test("the && operator should return falsified for a passing and failing property") {
    val gen = Gen.choose(1, 10)

    val lessThanTen = Prop.forAll(gen)(a => a < 10)
    val greaterThanTen = Prop.forAll(gen)(a => a > 10)

    val combined = lessThanTen && greaterThanTen

    val result = combined.run(100, RNG.Simple(12345))

    result.isFalsified should be(true)
  }

  test("the && operator should return falsified for two failing properties") {
    val gen = Gen.choose(1, 10)

    val lessThanZero = Prop.forAll(gen)(a => a < 10)
    val greaterThanTen = Prop.forAll(gen)(a => a > 10)

    val combined = lessThanZero && greaterThanTen

    val result = combined.run(100, RNG.Simple(12345))

    result.isFalsified should be(true)
  }

  test("the || operator should return passed for two properties that pass") {
    val gen = Gen.choose(0, 10)

    val lessThanTen = Prop.forAll(gen)(a => a < 10)
    val greaterThanZero = Prop.forAll(gen)(a => a > 0)

    val combined = lessThanTen || greaterThanZero

    val result = combined.run(100, RNG.Simple(12345))

    result.isFalsified should be(false)
  }

  test("the || operator should return passed for a passing and failing property") {
    val gen = Gen.choose(0, 10)

    val lessThanTen = Prop.forAll(gen)(a => a < 10)
    val greaterThanTen = Prop.forAll(gen)(a => a > 10)

    val combined = lessThanTen || greaterThanTen

    val result = combined.run(100, RNG.Simple(12345))

    result.isFalsified should be(false)
  }

  test("the || operator should return falsified for two failing properties") {
    val gen = Gen.choose(0, 10)

    val lessThanZero = Prop.forAll(gen)(a => a < 0)
    val greaterThanTen = Prop.forAll(gen)(a => a > 10)

    val combined = lessThanZero || greaterThanTen

    val result = combined.run(100, RNG.Simple(12345))

    result.isFalsified should be(true)
  }


}
