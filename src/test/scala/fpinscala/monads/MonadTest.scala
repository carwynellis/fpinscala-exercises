package fpinscala.monads

import fpinscala.state.{RNG, State}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

class MonadTest extends FunSuite with Matchers with MockitoSugar {

  test("sequence returns expected result for list monad") {
    val l = List(List(1), List(2), List(3))

    Monad.listMonad.sequence(l) should be(List(List(1,2,3)))
  }

  test("traverse returns expected result for list monad") {
    val l = List(List(1), List(2), List(3))

    val f = (l: List[Int]) => l.map(_ + 1)

    Monad.listMonad.traverse(l)(f) should be(List(List(2,3,4)))
  }

  test("replicateM returns expected result for list monad") {
    val l = List(1)

    Monad.listMonad.replicateM(3, l) should be(List(List(1, 1, 1)))
  }

  test("replicateM returns expected result for option monad") {
    val o = Some(1)

    Monad.optionMonad.replicateM(3, o) should be(Some(List(1, 1, 1)))
  }

  test("filterM returns expected result for list monad") {
    val l = List(1,2,3,4,5)

    val result = Monad.listMonad.filterM(l) { (i: Int) =>
      Monad.listMonad.unit(i % 2 == 0)
    }

    result should be(List(List(2,4)))
  }

  test("compose composes two kleisli arrows correctly for option monad") {
    val f = (i: Int) => Some(i + 1)
    val g = (i: Int) => Some(i * 2)

    Monad.optionMonad.compose(f, g)(1) should be(Some(4))
  }

  test("_flatMap returns expected result for list monad") {
    val l = List(1,2,3)

    Monad.listMonad._flatMap(l)((i: Int) => List(i + 1)) should be(List(2,3,4))
  }

  test("join returns a List(1) with given a List(List(1)) for the list monad") {
    val l = List(List(1))

    Monad.listMonad.join(l) should be(List(1))
  }

  test("__flatMap returns expected result for list monad") {
    val l = List(1,2,3)

    Monad.listMonad.__flatMap(l)((i: Int) => List(i + 1)) should be(List(2,3,4))
  }

  test("replicateM returns expected result for state monad") {
    val mockRNG = mock[RNG]

    val s = State.unit[RNG, Int](1)

    val (result, _) = Monad.stateMonad.replicateM(3, s).run(mockRNG)

    result should be(List(1,1,1))
  }

  test("sequence returns expected result for state monad") {
    val mockRNG = mock[RNG]

    val ls = List(
      State.unit[RNG, Int](1),
      State.unit[RNG, Int](2),
      State.unit[RNG, Int](3)
    )

    val (result, _) = Monad.stateMonad.sequence(ls).run(mockRNG)

    result should be(List(1,2,3))
  }

  test("map2 returns expected result for state monad") {
    val mockRNG = mock[RNG]

    val (result, _) = Monad.stateMonad.map2(
      State.unit[RNG, Int](1),
      State.unit[RNG, Int](2)
    )(_ + _)
    .run(mockRNG)

    result should be(3)
  }

  test("reader monad unit returns expected result") {
    val rm = Reader.readerMonad[String].unit(2)
    // Unit ignores the R argument returning the value passed in.
    rm.run("foo") should be(2)
  }

  test("reader monad flatMap returns expected result") {
    val slm = Reader((r: String) => r.length)

    Reader.readerMonad.flatMap(slm) { (i: Int) =>
      Reader.readerMonad.unit(i + 1)
    }.run("foo") should be(4)
  }

  test("reader monad sequence returns expected result") {
    // Sequence allows a single argument R to be applied to a number of
    // readers. The argument is passed to each reader in turn.
    // A rather contrived example that illustrates this.
    val l = List(
      Reader((r: String) => r.length),
      Reader((r: String) => if (r == "foo") 1 else 0),
      Reader((r: String) => if (r.contains("o")) 1 else 0)
    )

    // The result of each function is present in the result in the same order
    // as the input list of Readers.
    Reader.readerMonad.sequence(l).run("foo") should be(List(3, 1, 1))
  }
}
