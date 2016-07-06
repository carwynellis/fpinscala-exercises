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

  test("traverse returns a Right(List[B]) when no failures occurred") {
    val data = List(1,2,3)
    Either.traverse(data)((i: Int) => Right(i + 1)) should be(Right(List(2,3,4)))
  }

  test("traverse returns the first failure that occurred") {
    val data = List(1,2,3)
    Either.traverse(data)((i: Int) => Left(i)) should be(Left(1))
  }

  test("traverse returns only the failure if preceded by successful elements") {
    val data = List(1,2,3)
    Either.traverse(data)((i: Int) => if(i == 2) Left(i) else Right(i)) should be(Left(2))
  }

  test("sequence returns a Right(List[A]) if no Left() elements are present") {
    val data = List(Right(1), Right(2), Right(3))
    Either.sequence(data) should be(Right(List(1,2,3)))
  }

  test("sequence returns the first Left() in a list of Eithers") {
    val data = List(Left(1), Right(2), Right(3))
    Either.sequence(data) should be(Left(1))
  }

  test("sequence returns the first Left() preceded by Right() elements in a list of Either") {
    val data = List(Right(1), Left(2), Right(3))
    Either.sequence(data) should be(Left(2))
  }
}
