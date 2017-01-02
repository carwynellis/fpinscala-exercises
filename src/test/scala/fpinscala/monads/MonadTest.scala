package fpinscala.monads

import org.scalatest.{FunSuite, Matchers}

class MonadTest extends FunSuite with Matchers {

  test("sequence returns expected result for list monad") {
    val lm = Monad.listMonad

    val l = List(List(1), List(2), List(3))

    lm.sequence(l) should be(List(List(1,2,3)))
  }

  test("traverse returns expected result for list monad") {
    val lm = Monad.listMonad

    val l = List(List(1), List(2), List(3))

    val f = (l: List[Int]) => l.map(_ + 1)

    lm.traverse(l)(f) should be(List(List(2,3,4)))
  }

}
