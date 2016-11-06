package fpinscala.testing

import fpinscala.state.RNG
import org.mockito.Mockito._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}

class GenTest extends FunSuite with Checkers with MockitoSugar
  with GeneratorDrivenPropertyChecks with Matchers {

  override implicit val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 500,
    workers = 2
  )

  test("choose should return values within the specified range when run") {
    val min = 0
    val max = 100

    val chooser = Gen.choose(min, max)

    val mockRNG = mock[RNG]

    forAll { n: Int =>
      // Mocking RNG means we'll get stuck in a loop if we set the next value
      // to Int.MinValue
      whenever(n > Int.MinValue) {
        when(mockRNG.nextInt).thenReturn((n, mockRNG))
        val (result, _) = chooser.sample.run(mockRNG)
        result >= min && result < max
      }
    }
  }

  test("choose should return the first value when RNG.nextInt returns 0") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((0, mockRNG))

    val chooser = Gen.choose(1, 10)

    val (result, _) = chooser.sample.run(mockRNG)

    result should be(1)
  }

  test("choose should return the last value when RNG.nextInt returns Int.MaxValue") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((Int.MaxValue, mockRNG))

    val chooser = Gen.choose(1, 10)

    val (result, _) = chooser.sample.run(mockRNG)

    result should be(9)
  }

  test("unit returns the specified value when run") {
    val value = 1
    val genUnit = Gen.unit(value)
    val mockRNG = mock[RNG]

    val (result, _) = genUnit.sample.run(mockRNG)

    result should be(value)
  }

  test("boolean returns true when RNG returns an odd number") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((1, mockRNG))

    val (result, _) = Gen.boolean.sample.run(mockRNG)

    result should be(true)
  }

  test("boolean returns false when RNG returns an odd number") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((2, mockRNG))

    val (result, _) = Gen.boolean.sample.run(mockRNG)

    result should be(false)
  }

  test("listOfN returns a list of n generated values") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((1, mockRNG))

    val (result, _) = Gen.listOfN(10, Gen.boolean).sample.run(mockRNG)

    result should be(List.fill(10)(true))
  }

  test("nonWhitespaceChar returns a single non-whitespace character") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((0, mockRNG))

    val (result, _) = Gen.nonWhitespaceChar.sample.run(mockRNG)

    result should be('!')
  }

  test("map returns a new gen containing the result of the specified function") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((0, mockRNG))

    // The mocked RNG implementation will ensure choose always returns a 1
    val choose = Gen.choose(1, 10)

    // Map over the choose result with a simple function.
    val mapped = choose.map(_ + 1)

    val (result, _) = mapped.sample.run(mockRNG)

    result should be(2)
  }

  test("string returns a string of characters of the specified length") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((0, mockRNG))

    val length = 5

    val (result, _) = Gen.string(length).sample.run(mockRNG)

    result should be("!!!!!")
  }

  test("flatMap returns a new gen containing the result of the specified function") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((0, mockRNG))

    // The mocked RNG implementation will ensure choose always returns a 1
    val choose = Gen.choose(1, 10)

    // flatMap over the choose result with a simple function.
    val mapped = choose.flatMap(Gen.unit(_))

    val (result, _) = mapped.sample.run(mockRNG)

    result should be(1)
  }

  test("listOfN on Gen class produces expected result") {
    val mockRNG = mock[RNG]

    val elements = 3

    val genInt = Gen.unit[Int](elements)
    val genString = Gen.unit[String]("foo")

    val (result, _) = genString.listOfN(genInt).sample.run(mockRNG)

    result should be(List.fill(elements)("foo"))
  }

  test("union should combine two generators with equal likelihood") {
    val mockRNG = mock[RNG]

    // Mock up two responses to trigger selection of the first and second gen
    when(mockRNG.nextInt)
      .thenReturn((1, mockRNG))
      .thenReturn((Int.MaxValue, mockRNG))

    val g1 = Gen.unit[Int](1)
    val g2 = Gen.unit[Int](2)

    val union = Gen.union(g1, g2)

    val (result, _) = union.sample.run(mockRNG)

    result should be(1)

    val (secondResult, _) = union.sample.run(mockRNG)

    secondResult should be(2)
  }



}
