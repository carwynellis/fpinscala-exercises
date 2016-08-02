package fpinscala.state

import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

class StateTest extends FunSuite with Matchers with MockitoSugar {

  test("nonNegativeInt returns a positive int from RNG.nextInt unchanged") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt).thenReturn((10, mockRNG))

    RNG.nonNegativeInt(mockRNG) should be(10, mockRNG)
  }

  test("nonNegativeInt returns a positive number of the same magnitude when RNG.nextInt returns a negative int greater than Int.minValue") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt).thenReturn((-10, mockRNG))

    RNG.nonNegativeInt(mockRNG) should be(10, mockRNG)
  }

  test("nonNegativeInt returns a positive int where the first call to RNG.nextInt returns Int.MinValue which does not have a positive counterpart") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt)
      .thenReturn((-10, mockRNG))
      .thenReturn((10, mockRNG))

    RNG.nonNegativeInt(mockRNG) should be(10, mockRNG)
  }

}
