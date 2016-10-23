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

  test("choiceN chooses the correct par to run") {
    val parList = List(
      Nonblocking.Par.unit(0),
      Nonblocking.Par.unit(1),
      Nonblocking.Par.unit(2)
    )

    val choice = Nonblocking.Par.unit(1)

    val makeChoice = Nonblocking.Par.choiceN(choice)(parList)

    val result = Nonblocking.Par.run(es)(makeChoice)

    result should be(1)
  }

  test("choice chooses the first par when the predicate is true") {
    val predicate = Nonblocking.Par.unit(true)
    val truePar = Nonblocking.Par.unit("true")
    val falsePar = Nonblocking.Par.unit("false")

    val makeChoice = Nonblocking.Par.choice(predicate)(truePar, falsePar)

    val result = Nonblocking.Par.run(es)(makeChoice)

    result should be("true")
  }

  test("choice chooses the second par when the predicate is true") {
    val predicate = Nonblocking.Par.unit(false)
    val truePar = Nonblocking.Par.unit("true")
    val falsePar = Nonblocking.Par.unit("false")

    val makeChoice = Nonblocking.Par.choice(predicate)(truePar, falsePar)

    val result = Nonblocking.Par.run(es)(makeChoice)

    result should be("false")
  }

  test("choiceMap chooses the correct par to run") {
    val parMap = Map(
      "par0" -> Nonblocking.Par.unit(0),
      "par1" -> Nonblocking.Par.unit(1),
      "par2" -> Nonblocking.Par.unit(2)
    )

    val choice = Nonblocking.Par.unit("par1")

    val makeChoice = Nonblocking.Par.choiceMap(choice)(parMap)

    val result = Nonblocking.Par.run(es)(makeChoice)

    result should be(1)
  }

  test("chooser returns the expected result for given predicate and function") {
    val p = Nonblocking.Par.unit(true)

    val f = { c: Boolean =>
      if (c) Nonblocking.Par.unit(1)
      else Nonblocking.Par.unit(0)
    }

    val makeChoice = Nonblocking.Par.chooser(p)(f)

    val result = Nonblocking.Par.run(es)(makeChoice)

    result should be(1)
  }

  test("choice via chooser should return the correct par") {
    val p = Nonblocking.Par.unit(true)

    val truePar = Nonblocking.Par.unit(1)
    val falsePar = Nonblocking.Par.unit(0)

    val makeChoice = Nonblocking.Par.choiceViaChooser(p)(falsePar, truePar)

    val result = Nonblocking.Par.run(es)(makeChoice)

    result should be(1)
  }

  test("choiceNChooser chooses the correct par to run") {
    val parList = List(
      Nonblocking.Par.unit(0),
      Nonblocking.Par.unit(1),
      Nonblocking.Par.unit(2)
    )

    val choice = Nonblocking.Par.unit(1)

    val makeChoice = Nonblocking.Par.choiceNChooser(choice)(parList)

    val result = Nonblocking.Par.run(es)(makeChoice)

    result should be(1)
  }
}
