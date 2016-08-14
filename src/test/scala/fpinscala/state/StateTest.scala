package fpinscala.state

import fpinscala.state.RNG.Rand
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

  test("double returns a double less than 1 when RNG.nextInt returns Int.MaxValue") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt).thenReturn((Int.MaxValue, mockRNG))

    val (result, _) = RNG.double(mockRNG)

    result should be < 1.0
  }

  test("double returns zero when RNG.nextInt returns 0") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt).thenReturn((0, mockRNG))

    val (result, _) = RNG.double(mockRNG)

    result should be(0)
  }

  test("intDouble returns a tuple of (int, double)") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt)
      .thenReturn((1, mockRNG))
      .thenReturn((2, mockRNG))

    val (result, _) = RNG.intDouble(mockRNG)

    result should be(1, 2.0 / (Int.MaxValue.toDouble + 1.0))
  }

  test("doubleInt returns a tuple of (int, double)") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt)
      .thenReturn((1, mockRNG))
      .thenReturn((2, mockRNG))

    val (result, _) = RNG.doubleInt(mockRNG)

    result should be(2.0 / (Int.MaxValue.toDouble + 1.0), 1)
  }

  test("double3 returns a tuple of three doubles") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt)
      .thenReturn((1, mockRNG))
      .thenReturn((2, mockRNG))
      .thenReturn((3, mockRNG))

    val (result, _) = RNG.double3(mockRNG)

    result should be(
      1.0 / (Int.MaxValue.toDouble + 1.0),
      2.0 / (Int.MaxValue.toDouble + 1.0),
      3.0 / (Int.MaxValue.toDouble + 1.0)
    )
  }

  test("ints returns the specified number of random ints") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt)
      .thenReturn((3, mockRNG))
      .thenReturn((2, mockRNG))
      .thenReturn((1, mockRNG))

    val (result, _) = RNG.ints(3)(mockRNG)

    result should be(List(1,2,3))
  }

  test("doubleUsingMap returns expected result") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt)
      .thenReturn((1, mockRNG))

    val (result, _) = RNG.doubleUsingMap(mockRNG)(mockRNG)

    result should be(1.0 / (Int.MaxValue.toDouble + 1.0))
  }

  test("map2 can be used to combine two actions") {
    val mockRNG = mock[RNG]

    val (result, _) = RNG.map2(RNG.unit(1), RNG.unit(1))(_ + _)(mockRNG)

    result should be(2)
  }

  test("sequence combines a list of RNG transitions") {
    val mockRNG = mock[RNG]

    val transitionList = List(RNG.unit(1), RNG.unit(2), RNG.unit(3))

    val (result, _) = RNG.sequence(transitionList)(mockRNG)

    result should be(List(1,2,3))
  }

  test("flatmap returns a new state correctly") {
    val mockRNG = mock[RNG]

    val res = RNG.flatMap(RNG.unit(1)) { a =>
      RNG.unit(a + 1)
    }

    val (result, _) = res(mockRNG)

    result should be(2)
  }

  test("nonNegativeLessThan returns an int less than the specified value") {
    val mockRNG = mock[RNG]

    when(mockRNG.nextInt)
      .thenReturn((Int.MaxValue, mockRNG))
      .thenReturn((1, mockRNG))

    val (result, _) = RNG.nonNegativeLessThan(10)(mockRNG)

    result should be(1)
  }

  test("mapUsingFlatMap produces a new rng with a value modified by the specified function") {
    val mockRNG = mock[RNG]

    val (result, _) = RNG.mapUsingFlatMap(RNG.unit(1))( _ + 1 )(mockRNG)

    result should be(2)

  }

  test("map2UsingFlatMap can be used to combine two actions") {
    val mockRNG = mock[RNG]

    val (result, _) = RNG.map2UsingFlatMap(RNG.unit(1), RNG.unit(1))(_ + _)(mockRNG)

    result should be(2)
  }

}
