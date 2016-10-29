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

  test("choose should return values within the specified range") {
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
}
