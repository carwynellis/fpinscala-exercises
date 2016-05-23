package fpinscala.datastructures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("tail should return remaining elements of a list") {
    val l = List(1,2,3)
    assertResult(List(2,3)) { List.tail(l) }
  }

  test("tail should return Nil for tail of single element list") {
    val l = List(1)
    assertResult(Nil) { List.tail(l) }
  }

  test("tail should return Nil for tail of empty list") {
    val l = Nil
    assertResult(Nil) { List.tail(l) }
  }

  test("setHead should return a list with head set to specified valeue") {
    val l = List(1,2,3)
    val h = 5
    assertResult(List(5,2,3)) { List.setHead(l, h) }
  }

  test("drop should remove the first n elements of a list") {
    val l = List(1,2,3,4,5)
    assertResult(List(3,4,5)) { List.drop(l, 2) }
  }

  test("dropWhile should remove elements until predicate returns false") {
    val l = List(1,1,1,0,0,0)
    // Re-written passing predicate within a second parameter list to allow
    // type inference.
    assertResult(List(0,0,0)) { List.dropWhile(l)(_ == 1) }
  }

  test("init should remove last element of a list") {
    val l = List(1,2,3)
    assertResult(List(1,2)) { List.init(l) }
  }

  test("length should return the length of a list") {
    val l = List(1,2,3)
    assertResult(3) { List.length(l) }
  }

  test("foldLeft should return expected result for simple function") {
    val l = List(1,2,3)
    assertResult(6) { List.foldLeft(l, 0)( (x,y) => x + y) }
  }

  test("sumL computes the sum of a list") {
    val l = List(1,2,3)
    assertResult(6) { List.sumL(l) }
  }

  test("productL computes the product of a list") {
    val l = List(1.0, 2.0, 3.0)
    assertResult(6.0) { List.productL(l) }
  }

  test("reverse returns the reverse of a list") {
    val l = List(1,2,3)
    assertResult(List(3,2,1)) { List.reverse(l) }
  }

  test("foldLeftUsingRight returns expected result for a simple function") {
    val l = List(1,2,3)
    assertResult(6) { List.foldLeftUsingRight(l, 0)( (x,y) => x + y) }
  }

  test("foldRightUsingLeft returns expected result for a simple function") {
    val l = List(1,2,3)
    assertResult(6) { List.foldRightUsingLeft(l, 0)( (x,y) => x + y) }
  }

  test("appendF should produce a new list formed of first and second lists") {
    val l = List(1,2,3)
    val m = List(4,5,6)
    assertResult(List(1,2,3,4,5,6)) { List.appendF(l, m) }
  }
}
