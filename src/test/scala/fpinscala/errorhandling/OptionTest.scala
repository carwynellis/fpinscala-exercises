package fpinscala.errorhandling

import org.scalatest.{FunSuite, Matchers}

class OptionTest extends FunSuite with Matchers {

  val someInt = Some(1)

  test("map should return a new option modified by the specified function") {
    assertResult(Some("1")) { someInt.map(i => s"$i") }
  }

  test("map should return None when applied to an empty option") {
    assertResult(None) { None.map(i => s"$i") }
  }

  test("getOrElse should return option value where present") {
    assertResult(1) { someInt.getOrElse(0) }
  }

  test("getOrElse should return the default value where no value is present") {
    assertResult(0) { None.getOrElse(0) }
  }

  test("flatMap should return a new option containing the result of the specified function") {
    assertResult(Some("1")) { someInt.flatMap(i => Some(s"$i")) }
  }

  test("flatMap should return None when applied to an empty option") {
    assertResult(None) { None.flatMap(i => Some(s"$i")) }
  }

  test("orElse should return the value when applied to an option with a value") {
    assertResult(Some(1)) { someInt.orElse(Some(2)) }
  }

  test("orElse should return the option paramter when applied to an empty option") {
    assertResult(Some(2)) { None.orElse(Some(2)) }
  }

  test("filter should return current option with value if the value satisfies the predicate") {
    assertResult(Some(1)) { someInt.filter(i => i == 1) }
  }

  test("filter should return None if the current value does not satisfy the predicate") {
    assertResult(None) { someInt.filter(i => i > 10) }
  }

  test("filter should return None when applied to an empty option") {
    assertResult(None) { None.filter(i => 1 == 1) }
  }

  test("variance computes correct result for simple sequence") {
    val data = Seq(0.1, 0.2, 0.3)
    Option.variance(data).map { result =>
      result should be(0.00666 +- 0.0001)
    }
  }

  test("map2 applies function to two options correctly") {
    Option.map2(someInt, someInt)(_ + _) should be(Some(2))
  }

  test("map2 returns none if first option is none") {
    Option.map2(None, someInt)((x: Int, y: Int) => x + y) should be(None)
  }

  test("map2 returns none if second option is none") {
    Option.map2(someInt, None)((x: Int, y: Int) => x + y) should be(None)
  }

  test("sequence returns a list of values for a given list of options") {
    val list = List(Some(1), Some(2), Some(3))
    Option.sequence[Int](list) should be(Some(List(1,2,3)))
  }

  test("sequence returns none for a list containing a None") {
    val list = List(Some(1), None, Some(3))
    Option.sequence[Int](list) should be(None)
  }

  test("traverseWithMap returns a modified list wrapped in a Some() when all operations successful") {
    val list = List(1, 2, 3)
    Option.traverseWithMap(list)(i => Some(i * 2)) should be(Some(List(2,4,6)))
  }

  test("traverse returns a modified list wrapped in a Some() when all operations successful") {
    val list = List(1, 2, 3)
    Option.traverse(list)(i => Some(i * 2)) should be(Some(List(2,4,6)))
  }

  test("traverse returns none if one of the operations fails") {
    val list = List(1, 2, 3)
    Option.traverse(list)(i => if(i == 2) None else Some(i)) should be(None)
  }
}
