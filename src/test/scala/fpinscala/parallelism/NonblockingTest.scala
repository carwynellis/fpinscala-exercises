package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{AsyncFunSuite, Matchers}

class NonblockingTest extends AsyncFunSuite with Matchers with MockitoSugar {

  private val es: ExecutorService = Executors.newWorkStealingPool()

  test("run returns the result of the Par") {
    val par = Nonblocking.Par.map(Nonblocking.Par.unit(1)){ i => i }
    val result = Nonblocking.Par.run(es)(par)
    result should be(1)
  }

  test("run propagates exceptions thrown by Par") {
    // Setup a computation that should throw a div by zero exception.
    val parException = Nonblocking.Par.map(Nonblocking.Par.unit(0)) { _ =>
        1 / 0
    }

    an[Exception] should be thrownBy Nonblocking.Par.run(es)(parException)
  }
}
