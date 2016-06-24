package fpinscala.errorhandling

import org.scalatest.{FunSuite, Matchers}

class EitherTest extends FunSuite with Matchers {

  val leftVal = Left("foo")
  val rightVal = Right(1)

  test("map should map over the right value") {
    rightVal map { _ + 1 } should be(Right(2))
  }

  test("map of left should just return the left value") {
    leftVal map { i: Int => i + 1 } should be(leftVal)
  }

  test("flatMap of right should return mapped Right") {
    rightVal flatMap { i => Right( i + 1 ) } should be(Right(2))
  }

  test("flatMap of left should return unmodified left") {
    leftVal flatMap { i: Int => Right( i + 1 ) } should be(leftVal)
  }

  test("orElse should return Right value") {
    rightVal.orElse(Right(10)) should be(Right(1))
  }

  test("orElse on left should return specified else value") {
    leftVal.orElse(rightVal) should be(rightVal)
  }

  test("map2 applies function to two Right values correctly") {
    rightVal.map2(rightVal)(_ + _) should be(Right(2))
  }

  test("map2 returns left if applied to a left value") {
    leftVal.map2(rightVal)((a: Int, b: Int) => a + b) should be(leftVal)
  }

  test("map2 returns left if applied to a right value with a left value") {
    rightVal.map2(leftVal)((a: Int, b: Int) => a + b) should be(leftVal)
  }
}
