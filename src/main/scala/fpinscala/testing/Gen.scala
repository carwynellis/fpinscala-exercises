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
  def unit[A](a: => A): Gen[A] = ???
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

