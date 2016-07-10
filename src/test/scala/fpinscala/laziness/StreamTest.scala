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

}
