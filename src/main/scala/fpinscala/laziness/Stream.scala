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

  // Solutions using unfold for Exercise 5.13
  def mapUsingUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeUsingUnfold(n: Int): Stream[A] = unfold((n, this)) {
    case (c, Cons(h,t)) if c > 0 => Some(h(), (c - 1, t()))
    case _ => None
  }

  def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[S >: A, B](s: Stream[S])(f: (A,S) => B): Stream[B] = unfold(this, s) {
    case (Cons(a,b), Cons(x,y)) => Some( (f(a(), x()), (b(), y())) )
    case _ => None
  }

  // TODO - Urgh - parens hell :(
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this, s2) {
    case (Cons(a,b), Cons(x,y)) => Some( (Some(a()), Some(x())), (b(), y())   )
    case (Cons(a,b), _        ) => Some( (Some(a()),  None),     (b(), Empty) )
    case (_,         Cons(x,y)) => Some( (None, Some(x())),      (Empty, y()) )
    case (Empty, Empty)         => None
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    def loop(startStream: Stream[B], thisStream: Stream[A]): Boolean = (startStream, thisStream) match {
      case (Cons(a,b), Cons(x,y)) if a() == x() => loop(b(), y())
      case (Cons(a,b), Cons(x,y)) if a() != x() => false
      // If we exhaust the stream passed in then it must be a match
      case (Empty, _) => true
      // Otherwise 'this' stream was exhausted first so cannot start with the stream passed in
      case _ => false
    }

    loop(s, this)
  }

  // Use a tuple to carry a flag indicating when we've reached the end of the stream so we can terminate the unfold.
  // This allows the empty stream to be emitted whilst ensuring the unfold terminates.
  def tails: Stream[Stream[A]] = unfold((false, this)) {
    case (false, Cons(h, t)) => Some(cons(h(), t()), (false, t()))
    case (false, Empty)      => Some(Stream(), (true, Empty))
    case (true, _)           => None
    case (false, _)          => None
  }

  def scanRight[B](z: B)(f: (A,B) => B): Stream[B] =
    foldRight[Stream[B]](Stream(z))((elem, acc) =>
      acc match {
        case Cons(h, _) => Cons(() => f(elem, h()), () => acc)
      }
    )

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

  def fibs(a: Int = 0, b: Int = 1): Stream[Int] = Stream.cons(a, fibs(b, a+b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty
  }

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(_ => Option(a, a))

  def onesUsingUnfold: Stream[Int] = constantUsingUnfold(1)

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def fibsUsingUnfold(a: Int = 0, b: Int = 1): Stream[Int] = unfold(a, b)(t => Some(t._1, (t._2, t._1 + t._2)))
}