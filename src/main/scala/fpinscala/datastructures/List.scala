package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  // This implementation is not tail recursive
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil // TODO - could error out here instead
    case Cons(_, Nil) => Nil // Discard the last element
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_, a) => a + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  def sumL(l: List[Int]) = foldLeft(l, 0)((x,y) => x + y)

  def productL(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l: List[A], List[A]())((accumulator: List[A], element: A) => Cons(element, accumulator))

  // Solutions have a different implementation. Note that reverse is implemented using foldLeft!
  // See - https://github.com/fpinscala/fpinscala/blob/master/answerkey/datastructures/13.answer.scala
  def foldLeftUsingRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((b,a) => f(a,b))

  def foldRightUsingLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b,a) => f(a,b))

  def appendF[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((e: A, acc: List[A]) => Cons(e, acc))

  def concatLists[A](l: List[List[A]]): List[A] =
    foldRightUsingLeft(l, List[A]())((acc: List[A], elem: List[A]) => append(acc, elem))

  def addOneToIntList(l: List[Int]): List[Int] =
    foldRightUsingLeft(l, List[Int]())((e: Int, acc: List[Int]) => Cons(e+1, acc))

  def listDoubleToListString(l: List[Double]): List[String] =
    foldRightUsingLeft(l, List[String]())((e: Double, acc: List[String]) => Cons(s"$e", acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightUsingLeft(l, List[B]())((e: A, acc: List[B]) => Cons(f(e), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightUsingLeft(as, List[A]())((e: A, acc: List[A]) => if(f(e)) acc else Cons(e, acc))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concatLists(map(as)(f))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(e => if (f(e)) Nil else List(e))

  def head[A](l: List[A]): A = l match {
    case Cons(h, _) => h
    case Nil => throw new NoSuchElementException
  }

  // TODO - would be nicer to foldRight so no reverse is required
  def combineAndSumLists(a1: List[Int], a2: List[Int]): List[Int] = {
    @tailrec
    def loop(l: List[Int], m: List[Int], acc: List[Int]): List[Int] = l match {
      case Cons(h, _) => loop(tail(l), tail(m), Cons(h + head(m), acc))
      case Nil => acc
    }
    reverse(loop(a1, a2, List()))
  }

  // TODO - would be nicer to foldRight so no reverse is required
  def zipWith[A,B](a1: List[A], a2: List[A])(f: (A,A) => B): List[B] = {
    @tailrec
    def loop(l: List[A], m: List[A], acc: List[B]): List[B] = l match {
      case Cons(h, _) => loop(tail(l), tail(m), Cons(f(h, head(m)), acc))
      case Nil => acc
    }
    reverse(loop(a1, a2, List()))
  }
}
