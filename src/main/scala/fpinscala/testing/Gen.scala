package fpinscala.testing

import fpinscala.state._
import fpinscala.laziness.Stream
import fpinscala.testing.Prop._

/**
  * The library developed in this chapter goes through several iterations. This
  * file is just the shell, which you can fill in and modify while working
  * through the chapter.
  */

case class Prop(label: String, run: (TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop(s"$label && ${p.label}", { (n, rng) =>
    run(n, rng) match {
      case Passed => p.run(n, rng)
      case falsified => falsified
      }
    })

  def ||(p: Prop) = Prop(s"$label || ${p.label}", { (n, rng) =>
    run(n, rng) match {
      case Falsified(message, successes) => tagWithPreviousFailure(message, p, n, rng)
      case passed => passed
    }
  })

  // Only really applies to the || operator. If both fail then ensure we
  // output both failure reasons.
  def tagWithPreviousFailure(failure: String, p: Prop, n: TestCases, rng: RNG) = {
    p.run(n, rng) match {
      case Falsified(message, successes) => Falsified(
        s"$failure\n$message", successes
      )
      case passed => passed
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A], label: String)(f: A => Boolean): Prop = Prop(label, {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a))
          Passed
        else
          Falsified(s"Property: $label failed on value: $a", i)
      } catch { case e: Exception => Falsified(buildMsg(label, a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  })

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](property: String, s: A, e: Exception): String =
    s"property: $property\n" +
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace: \n ${e.getStackTrace.mkString("\n")}"
}


object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  // For now assume positive integers only.
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    // Map 0 -> Int.MaxValue onto the scale start - stopExclusive
    val step = Int.MaxValue / (stopExclusive - start - 1)
    val state = State(RNG.nonNegativeInt) map { n =>
      // Use this step to map n to our scale
      start + (n / step)
    }
    Gen(state)
  }

  def boolean: Gen[Boolean] = {
    val state = State(RNG.nonNegativeInt) map { n =>
      // Map odd / even to true / false
      n % 2 == 1
    }
    Gen(state)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    // Build a List of Gen[A] and then use sequence to obtain a Gen[List[A]]
    Gen(State.sequence(List.fill(n)(g.sample)))

  def nonWhitespaceChar = choose(33, 127) map { n: Int => n.toChar }

  def string(n: Int): Gen[String] =
    Gen(State.sequence(List.fill(n)(nonWhitespaceChar.sample)).map {
      l: List[Char] => l.mkString
    })

  // Select the generator randomly according to whether the next random int is
  // odd or even
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // Use choose to select a number from 0 to Int.MaxValue
  // Let w = (Int.MaxValue * g1.weight)
  // Let n = next result of choose
  // If n < w select g1, otherwise select g2.
  // This ensures that generators are selected with a weighted bias.
  // Assumes that the sum of the weights = 1
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1UpperBound = (g1._2 * Int.MaxValue).toInt
    choose(0, Int.MaxValue) flatMap { n =>
      if (n < g1UpperBound) g1._1 else g2._1
    }
  }

}

case class SGen[A](forSize: Int => Gen[A]) {

  def apply(size: Int): Gen[A] = forSize(size)

  def map[B](f: A => B): SGen[B] = SGen { i => forSize(i).map(f) }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { i =>
    forSize(i).flatMap { v =>
      val res = f(v)
      res(i)
    }
  }

}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { i => Gen.listOfN(i, g) }
}

case class Gen[A](sample: State[RNG,A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map[B](f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap[B](a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n => Gen.listOfN(n, this) }

  def unsized: SGen[A] = SGen(_ => this)
}

