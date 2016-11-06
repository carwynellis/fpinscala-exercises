package fpinscala.testing

import fpinscala.state._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase,SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

}

trait SGen[+A] {

}

case class Gen[A](sample: State[RNG,A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map[B](f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap[B](a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n => Gen.listOfN(n, this) }

}

