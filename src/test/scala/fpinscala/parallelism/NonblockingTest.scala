package fpinscala.parallelism

import java.util.concurrent.Executors

import org.scalatest.{AsyncFunSuite, Matchers}

class NonblockingTest extends AsyncFunSuite with Matchers {

  private val es = Executors.newWorkStealingPool()

  test("run returns the result of the Par") {
    val par = Nonblocking.Par.unit(1)

    val result = Nonblocking.Par.run(es)(par)

    result should be(1)
  }

  test("run propagates exceptions thrown by Par") {
    // Setup a computation that should throw a div by zero exception.
    val parException = Nonblocking.Par.map(Nonblocking.Par.unit(0)) { arg =>
      println(s"Attempting to divide 1 by $arg")
      try {
        1 / 0
      }
      catch {
        case e: Exception =>
          println(s"Caught exception: $e - ${e.printStackTrace()})")
      }
    }

    // TODO - The exception is swallowed in the other thread so we continue to
    // block
    an[Exception] should be thrownBy Nonblocking.Par.run(es)(parException)
  }
}
