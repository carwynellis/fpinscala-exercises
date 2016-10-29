package fpinscala.testing

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
  * For Exercise 8.1 define some properties for a function
  *
  *   sum: List[Int] => Int
  */
class ScalaCheckSumSpec extends FunSuite with Checkers {

  test("sum of a list should be the same as the sum of that list reversed") {
    check { (l: List[Int]) => l.sum == l.reverse.sum }
  }

  test("sum of a list where elements the same should be list.length * elemValue") {
    // For now just use a static list size, could generate variable lengths within reasonable constraints.
    val length = 10
    check { (n: Int) => List.fill(length)(n).sum == length * n }
  }

  test("sum of empty list should be zero") {
    // Doesn't seem necessary to involve scalacheck here.
    List.empty[Int].sum == 0
  }

  test("negative values should be deducted from the sum correctly") {
    // Hmm...getting a little contrived perhaps...
    check { (n: Int) => List(n, -n).sum == 0 }
  }

}
