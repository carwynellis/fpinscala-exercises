package fpinscala.testing

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}

/**
  * For Exercise 8.2 define some properties for a function
  *
  *   max: List[Int] => Int
  */
class MaxPropertiesTest extends FunSuite with Checkers with GeneratorDrivenPropertyChecks with Matchers {

  test("max of list of elements with same value should be that value") {
    check { n: Int => List.fill(10)(n).max == n }
  }

  test("max of two lists should be the same irrespective of list ordering") {
    forAll { l: List[Int] =>
      // max only defined for non-empty lists
      whenever(l.nonEmpty) {
        l.max == l.reverse.max
      }
    }
  }

  test("max of list should be the maximum value of that list") {
    forAll { l: List[Int] =>
      // max only defined for non-empty lists
      whenever(l.nonEmpty) {
        l.max == l.sorted.last
      }
    }
  }

  test("max of empty list should throw an exception") {
    an[UnsupportedOperationException] should be thrownBy List.empty[Int].max
  }

  test("max of a list should be a value in that list") {
    forAll { l: List[Int] =>
      // max only defined for non-empty lists
      whenever(l.nonEmpty) {
        l.contains(l.max)
      }
    }
  }

}
