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

  test("headOption should return a Some for a stream with a least one value") {
    stream.headOption should be(Some(1))
  }

  test("headOption should return a None for an empty stream") {
    val empty = Stream()
    empty.headOption should be(None)
  }

  test("map returns a new stream with values modified by the specified mapping function") {
    stream.map(i => i + 1).toList should be(List(2,3,4))
  }

  test("filter returns a stream containing only those elements satisfying the predicate function") {
    stream.filter(_ < 3).toList should be(List(1,2))
  }

  test("append returns a new stream composed of the current and specified streams combined") {
    val secondStream = Stream(4,5,6)
    stream.append(secondStream).toList should be(List(1,2,3,4,5,6))
  }

  test("flatMap returns a new stream of values modified by the specified function") {
    stream.flatMap(i => Stream(i + 1)).toList should be(List(2,3,4))
  }

  test("constant returns an infinite stream of the specified element") {
    Stream.constant("foo").take(3).toList should be(List("foo", "foo", "foo"))
  }

  test("from returns an infinite stream of integers increasing by 1 at a time") {
    Stream.from(1).take(5).toList should be(List(1,2,3,4,5))
  }

  test("fibs returns an infinite stream of fibonacci numbers") {
    Stream.fibs().take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  test("unfold returns a new stream generated using the specified state and next state function") {
    Stream.unfold[Int,Int](0)(s => Some(s+1, s+1)).take(3).toList should be(List(1,2,3))
  }
}
