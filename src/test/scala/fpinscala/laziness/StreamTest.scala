package fpinscala.laziness

import org.scalatest.{FunSuite, Matchers}

class StreamTest extends FunSuite with Matchers {

  test("toList should convert a stream of values to equivalent list of values") {
    val stream = Stream(1,2,3)
    stream.toList should be(List(1,2,3))
  }

}
