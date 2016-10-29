package fpinscala.state

import fpinscala.state.RNG.Rand
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
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

  test("State.unit returns a new state containing the specififed value") {
    val mockRNG = mock[RNG]

    val (result, _) = State.unit[RNG, Int](1).run(mockRNG)

    result should be(1)
  }

  test("State.flatMap returns a new state with a value modified by the specified function") {
    val mockRNG = mock[RNG]

    val initialState = State.unit[RNG, Int](1)

    val (result, _) = initialState.flatMap[Int] { a =>
      State.unit(a + 1)
    }.run(mockRNG)

    result should be(2)
  }

  test("State.map returns a new state with the value modified by the specified function") {
    val mockRNG = mock[RNG]

    val initialState = State.unit[RNG, Int](1)

    val (result, _) = initialState.map[Int] { _ + 1 }.run(mockRNG)

    result should be(2)
  }

  test("State.map2 can be used to combine two actions") {
    val mockRNG = mock[RNG]

    val (result, _) = State.unit[RNG, Int](1).map2(State.unit[RNG, Int](2))(_ + _).run(mockRNG)

    result should be(3)
  }

  test("State.sequence combines a list of actions") {
    val mockRNG = mock[RNG]

    val transitionList = List(State.unit[RNG, Int](1), State.unit[RNG, Int](2), State.unit[RNG, Int](3))

    val (result, _) = State.sequence(transitionList).run(mockRNG)

    result should be(List(1,2,3))
  }

  test("machine that is out of candy ignores all inputs") {
    val machine = Machine(locked = true, candies = 0, coins = 0)

    // Attempt to unlock machine and retrieve a candy
    val inputs = List(Coin, Turn)

    val result = State.simulateMachine(inputs).run(machine)

    result should be((0,0), machine)
  }

  test("inserting a coin into a locked machine unlocks it if candies remain") {
    val machine = Machine(locked = true, candies = 10, coins = 0)

    val inputs = List(Coin)

    val result = State.simulateMachine(inputs).run(machine)

    result should be((10,1), Machine(false, 10, 1))
  }

  test("turning the knob on an unlocked machine will cause it to dispense candy and become locked") {
    val machine = Machine(locked = false, candies = 10, coins = 1)

    val inputs = List(Turn)

    val result = State.simulateMachine(inputs).run(machine)

    result should be((9,1), Machine(true, 9, 1))
  }

  test("turning the knob on a locked machine does nothing") {
    val machine = Machine(locked = true, candies = 10, coins = 1)

    val inputs = List(Turn)

    val result = State.simulateMachine(inputs).run(machine)

    result should be((10,1), machine)
  }

  test("inserting a coin into an unlocked machine does nothing") {
    val machine = Machine(locked = false, candies = 10, coins = 1)

    val inputs = List(Coin)

    val result = State.simulateMachine(inputs).run(machine)

    result should be((10,1), machine)
  }

  test("simulateMachine returns expected final state for sequence of inputs") {
    val machine = Machine(locked = true, candies = 2, coins = 0)

    // Run three coin, turn sequences which should result in an empty machine with 2 coins
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn)

    val result = State.simulateMachine(inputs).run(machine)

    result should be((0,2), Machine(locked = true, candies = 0, coins = 2))
  }

  test("simulateMachineWithForComprehension returns expected final state") {
    val machine = Machine(locked = true, candies = 2, coins = 0)

    // Run three coin, turn sequences which should result in an empty machine with 2 coins
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn)

    val result = State.simulateMachineWithForComprehension(inputs).run(machine)

    result should be((0,2), Machine(locked = true, candies = 0, coins = 2))
  }

}
