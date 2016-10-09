package fpinscala.parallelism

import java.util.concurrent.{Executors, TimeUnit}

import fpinscala.parallelism.Par.Par
import org.scalatest.{AsyncFunSuite, Matchers}

class ParTest extends AsyncFunSuite with Matchers {

  private val executorService = Executors.newWorkStealingPool()

  test("asyncF evaluates specified function asynchronously") {
    val asyncF: (Int) => Par[Int] = Par.asyncF[Int, Int](_ + 1)

    val f = asyncF(1)(executorService)

    f.get(1, TimeUnit.SECONDS) should be(2)
  }

}
