package fpinscala.testing

import Gen._
import SGen._
import Prop._
import fpinscala.parallelism.Nonblocking._

object GenSpecifications extends App {

  def maxOfListExampleProperty() = {
    // Example property from section 8.4.1 - Some simple examples
    val smallInt = choose(-10, 10)

    val maxProp = forAll(
      listOfAtLeastOne(smallInt),
      "max of list should be greater than or equal to every other element") {
        ns: List[Int] =>
          val max = ns.max
          !ns.exists(_ > max)
      }

    run(maxProp)
  }

  def sortedListProperty() = {
    val ints = choose(-100, 100)

    val sortedProp = forAll(
      SGen.listOfAtLeastOne(ints),
      "List.sorted should always yield a sorted list") { l: List[Int] =>
        val sorted = l.sorted
        // Sorted list should be of the same size as the original input list
        // Sorted list head should be less than or equal to sorted list tail
        (sorted.head <= sorted.last) &&
        // Successive pairs of elements should be in sorted order
        ! (sorted.zip(sorted.tail) exists { case (a: Int, b: Int) => a > b })
      }

    run(sortedProp)
  }

  def simpleCheckExample() = {
    val falseOrTrueCheck = Prop.check({false || true }, "f || t yields t")

    run(falseOrTrueCheck)
  }

  def checkParMapProperty() = {
    val mapProperty = checkPar( {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }, "mapping a Par should return a Par containing the result of the specified function")

    run(mapProperty)
  }

  def checkParForkProperty() = {
    val forkProp = forAllPar(pint2, "fork(x) should equal x") { n  =>
      equal(
        Par.fork(n),
        n
      )
    }

    run(forkProp)
  }

  maxOfListExampleProperty()
  sortedListProperty()
  simpleCheckExample()
  checkParMapProperty()
  checkParForkProperty()

  shutdownExecutors
}
