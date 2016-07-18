package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the function `f`
  // takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    // Here `b` is the unevaluated recursive step that folds the tail of the stream.
    // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List.empty[A])((elem, acc) => elem :: acc)

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) Empty else cons[A](h(), t().take(n - 1))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 1) t() else t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if(p(h())) cons[A](h(), t().takeWhile(p)) else Empty
  }

  def takeWhileFoldR(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a,b) => if(p(a)) cons(a, b) else Empty )

  @annotation.tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if(p(h())) t().forAll(p) else false
  }

  def forAllFoldR(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a,b) => if (p(a)) cons(a,b) else b)

  // Since a stream is either a cons or Empty need to modify type bounds to allow super classes.
  def append[S >: A](s: Stream[S]): Stream[S] =
    foldRight(s)((a, b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a,b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}