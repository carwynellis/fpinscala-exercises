package fpinscala.testing

import fpinscala.state.RNG
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

class SGenTest extends FunSuite with MockitoSugar with Matchers {

  test("map returns a new SGen containing the reuslt of the specified function") {
    val mockRNG = mock[RNG]

    val sGen = Gen.unit(1).unsized

    val mapped = sGen.map(i => i + 1)

    val (result, _) = mapped(1).sample.run(mockRNG)

    result should be(2)
  }

  test("flatMap returns the result of the specified function") {
    val mockRNG = mock[RNG]

    val sGen = Gen.unit(1).unsized

    val flatMapped = sGen.flatMap(i => Gen.unit(i + 1).unsized)

    val (result, _) = flatMapped(1).sample.run(mockRNG)

    result should be(2)
  }

  test("listOf should return an SGen that generates lists of the requested size") {
    val mockRNG = mock[RNG]

    val sGen = SGen.listOf(Gen.unit(1))

    val (result, _) = sGen(5).sample.run(mockRNG)

    result should be(List.fill(5)(1))
  }

  test("listOfAtLeastOne should return an SGen that generates a single element list if 0 elements requested") {
    val mockRNG = mock[RNG]

    val sGen = SGen.listOfAtLeastOne(Gen.unit(1))

    val (result, _) = sGen(0).sample.run(mockRNG)

    result should be(List(1))
  }
}
