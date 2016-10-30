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
    val step = Int.MaxValue / (stopExclusive - start)
    val state = State(RNG.nonNegativeInt) map { n =>
      // Use this step to map n to our scale
      n / step
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

}

// TODO - is this needed anymore?
//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

case class Gen[A](sample: State[RNG,A]) {
  // Moved here from trait to satisfy deps in Monad code.
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

