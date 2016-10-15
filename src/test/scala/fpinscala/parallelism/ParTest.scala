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


}
