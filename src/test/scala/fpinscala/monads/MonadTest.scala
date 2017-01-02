package fpinscala.monads

import org.scalatest.{FunSuite, Matchers}

class MonadTest extends FunSuite with Matchers {

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
}
