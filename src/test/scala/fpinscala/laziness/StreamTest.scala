package fpinscala.laziness

import org.scalatest.{FunSuite, Matchers}

class StreamTest extends FunSuite with Matchers {

  val stream = Stream(1,2,3)

  test("toList should convert a stream of values to equivalent list of values") {
    stream.toList should be(List(1,2,3))
  }

  test("take should return the first n elements of a stream") {
    stream.take(2).toList should be(List(1,2))
  }

  test("drop should drop the first n elements of a stream") {
    stream.drop(2).toList should be(List(3))
  }

  test("takeWhile should return all elements until the predicate is false") {
    stream.takeWhile(_ < 3).toList should be(List(1,2))
  }

  test("forAll should return true where the predicate applies to all elements") {
    stream.forAll(_ < 10) should be(true)
  }

  test("forAll should return false where the predicate does not apply to all elements") {
    stream.forAll(_ < 3) should be(false)
  }

  test("forAllFoldR should return true where the predicate applies to all elements") {
    stream.forAllFoldR(_ < 10) should be(true)
  }

  test("forAllFoldR should return false where the predicate does not apply to all elements") {
    stream.forAllFoldR(_ < 3) should be(false)
  }

  test("takeWhileFoldR should return all elements until the predicate is false") {
    stream.takeWhileFoldR(_ < 3).toList should be(List(1,2))
  }
}
