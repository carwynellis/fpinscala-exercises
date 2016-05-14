package fpinscala.gettingstarted

import fpinscala.gettingstarted.PolymorphicFunctions.isSorted
import org.scalatest.FunSuite

class PolymorphicFunctionsTest extends FunSuite {

  def ascendingIntOrder(a: Int, b: Int) = a < b

  test("isSorted should return true for a sorted array") {
    val sorted = Array(1,2,3)
    assertResult(true) { isSorted(sorted, ascendingIntOrder) }
  }

  test("isSorted should return false for an array that is not sorted") {
    val unsorted = Array(1,5,2,3)
    assertResult(false) { isSorted(unsorted, ascendingIntOrder) }
  }

  test("isSorted should return true for arrays containing zero or 1 elements") {
    val empty = Array()
    val singleElement = Array(1)

    assertResult(true) { isSorted(empty, ascendingIntOrder) && isSorted(singleElement, ascendingIntOrder) }
  }
}
