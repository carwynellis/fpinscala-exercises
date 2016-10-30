package fpinscala.testing

import fpinscala.state.RNG
import org.mockito.Mockito._
import org.scalatest.FunSuite
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}

class GenTest extends FunSuite with Checkers with MockitoSugar
  with GeneratorDrivenPropertyChecks {

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

  test("unit returns the specified value when run") {
    val value = 1
    val genUnit = Gen.unit(value)
    val mockRNG = mock[RNG]

    val (result, _) = genUnit.sample.run(mockRNG)

    result === value
  }

  test("boolean returns true when RNG returns an odd number") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((1, mockRNG))

    val (result, _) = Gen.boolean.sample.run(mockRNG)

    result === true
  }

  test("boolean returns false when RNG returns an odd number") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((2, mockRNG))

    val (result, _) = Gen.boolean.sample.run(mockRNG)

    result === false
  }

  test("listOfN returns a list of n generated values") {
    val mockRNG = mock[RNG]
    when(mockRNG.nextInt).thenReturn((1, mockRNG))

    val (result, _) = Gen.listOfN(10, Gen.boolean).sample.run(mockRNG)

    result === List.fill(10)(true)
  }

}
