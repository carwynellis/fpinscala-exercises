package fpinscala.testing

import fpinscala.state.RNG
import fpinscala.testing.Prop.Falsified
import org.scalatest.{FunSuite, Matchers}

class PropTest extends FunSuite with Matchers {

  test("the && operator should return passed for two properties that pass") {
    val gen = Gen.choose(1, 10)

    val lessThanTen = Prop.forAll(gen, "less than 10")(a => a < 10)
    val greaterThanZero = Prop.forAll(gen, "greater than 0")(a => a > 0)

    val combined = lessThanTen && greaterThanZero

    val result = combined.run(100, 100, RNG.Simple(12345))

    result.isFalsified should be(false)
  }

  test("the && operator should return falsified for a passing and failing property") {
    val gen = Gen.choose(1, 10)

    val lessThanTen = Prop.forAll(gen, "less than 10")(a => a < 10)
    val greaterThanTen = Prop.forAll(gen, "greater than 10")(a => a > 10)

    val combined = lessThanTen && greaterThanTen

    val result = combined.run(100, 100, RNG.Simple(12345))

    result.isFalsified should be(true)
  }

  test("the && operator should return falsified for two failing properties") {
    val gen = Gen.choose(1, 10)

    val lessThanZero = Prop.forAll(gen, "less than 0")(a => a < 10)
    val greaterThanTen = Prop.forAll(gen, "greater than 10")(a => a > 10)

    val combined = lessThanZero && greaterThanTen

    val result = combined.run(100, 100, RNG.Simple(12345))

    result.isFalsified should be(true)
  }

  test("the || operator should return passed for two properties that pass") {
    val gen = Gen.choose(0, 10)

    val lessThanTen = Prop.forAll(gen, "less than 10")(a => a < 10)
    val greaterThanZero = Prop.forAll(gen, "greater than 0")(a => a > 0)

    val combined = lessThanTen || greaterThanZero

    val result = combined.run(100, 100, RNG.Simple(12345))

    result.isFalsified should be(false)
  }

  test("the || operator should return passed for a passing and failing property") {
    val gen = Gen.choose(0, 10)

    val lessThanTen = Prop.forAll(gen, "less than 10")(a => a < 10)
    val greaterThanTen = Prop.forAll(gen, "greater than 10")(a => a > 10)

    val combined = lessThanTen || greaterThanTen

    val result = combined.run(100, 100, RNG.Simple(12345))

    result.isFalsified should be(false)
  }

  test("the || operator should return falsified for two failing properties") {
    val gen = Gen.choose(0, 10)

    val lessThanZero = Prop.forAll(gen, "less than 0")(a => a < 0)
    val greaterThanTen = Prop.forAll(gen, "greater than 10")(a => a > 10)

    val combined = lessThanZero || greaterThanTen

    val result = combined.run(100, 100, RNG.Simple(12345))

    result.isFalsified should be(true)

    // Additional coverage around failure tagging
    // TODO - this is ugly :(
    val falsified = result.asInstanceOf[Falsified]

    falsified.failure should include("less than 0")
    falsified.failure should include("greater than 10")
  }


}
