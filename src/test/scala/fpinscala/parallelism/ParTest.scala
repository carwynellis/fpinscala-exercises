package fpinscala.parallelism

import java.util.concurrent.{Executors, TimeUnit, Future}

import fpinscala.parallelism.Par.Par
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.TimeoutException

class ParTest extends AsyncFunSuite with Matchers {

  private val executorService = Executors.newWorkStealingPool()

  // Note that the map2 tests implicitly test the TimeoutFuture.
  test("map2 applies the specified function to the specified pars") {
    val par = Par.unit[Int](1)

    val result = Par.run(executorService)(Par.map2(par,par)(_ + _))

    result.get(1, TimeUnit.SECONDS) should be (2)
  }

  test("map2 throws TimeoutException if futures don't complete in time") {
    val par = Par.lazyUnit[Int] {
      Thread.sleep(50)
      1
    }

    val result = Par.run(executorService)(Par.map2(par,par)(_ + _))

    a[TimeoutException] should be thrownBy result.get(
      5,
      TimeUnit.NANOSECONDS
    )
  }

  test("asyncF evaluates specified function asynchronously") {
    val asyncF: (Int) => Par[Int] = Par.asyncF[Int, Int](_ + 1)

    val f = asyncF(1)(executorService)

    f.get(1, TimeUnit.SECONDS) should be(2)
  }


  test("sequence takes a list of par, and returns a par of list") {
    val intList = List.range(1,5)
    val parList: List[Par[Int]] = intList.map(Par.unit[Int])

    val result = Par.run(executorService)(Par.sequence(parList))

    result.get should be(intList)
  }

  test("parMap returns input list modified by specified function") {
    val intList = List.range(1,5)

    val result = Par.run(executorService)(Par.parMap(intList)(_ + 1))

    result.get should be(List.range(2,6))
  }

  test("filter returns only list elements satisfying the predicate") {
    val result = Par.run(executorService)(Par.parFilter(List.range(1,10))(_ <= 5))

    result.get should be(List.range(1,6))
  }
}
