package fpinscala.errorhandling

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  val someInt = Some(1)
  val none = None

  test("map should return a new option modified by the specified function") {
    assertResult(Some("1")) { someInt.map(i => s"$i") }
  }

  test("map should return None when applied to an empty option") {
    assertResult(None) { none.map(i => s"$i") }
  }

  test("getOrElse should return option value where present") {
    assertResult(1) { someInt.getOrElse(0) }
  }

  test("getOrElse should return the default value where no value is present") {
    assertResult(0) { none.getOrElse(0) }
  }

  test("flatMap should return a new option containing the result of the specified function") {
    assertResult(Some("1")) { someInt.flatMap(i => Some(s"$i")) }
  }

  test("flatMap should return None when applied to an empty option") {
    assertResult(None) { none.flatMap(i => Some(s"$i")) }
  }

  test("orElse should return the value when applied to an option with a value") {
    assertResult(Some(1)) { someInt.orElse(Some(2)) }
  }

  test("orElse should return the option paramter when applied to an empty option") {
    assertResult(Some(2)) { none.orElse(Some(2)) }
  }

  test("filter should return current option with value if the value satisfies the predicate") {
    assertResult(Some(1)) { someInt.filter(i => i == 1) }
  }

  test("filter should return None if the current value does not satisfy the predicate") {
    assertResult(None) { someInt.filter(i => i > 10) }
  }

  test("filter should return None when applied to an empty option") {
    assertResult(None) { none.filter(i => 1 == 1) }
  }
}
