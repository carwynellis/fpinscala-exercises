package fpinscala.applicative

import org.scalatest.{FunSuite, Matchers}

class ApplicativeTest extends FunSuite with Matchers {

  // For Exercise 12.4
  // Demonstrates that stream applicative sequence transposes the input
  // streams such that the first row of values becomes the first column of
  // values in the output.
  test("stream applicative sequence should return expected result") {
    val sa = Applicative.streamApplicative

    val ls = List(
      Stream(1,2,3),
      Stream(4,5,6),
      Stream(7,8,9)
    )

    sa.sequence(ls) should be(Stream(
      List(1,4,7),
      List(2,5,8),
      List(3,6,9)
    ))

  }

}
