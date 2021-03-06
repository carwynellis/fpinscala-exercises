package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((elem, acc) => map2(f(elem), acc)(_ :: _))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    traverse(lma)(identity)

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def product[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  // Tried using traverse with inner map/flatmap but there's no way to
  // represent the empty value for the false case so looks like some sort of
  // recursive approach is needed for now.
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h)) { b =>
      // Filter returned a true value so retain the current value.
      if (b) map(filterM(t)(f))(h :: _)
      // Filter returned false so continue the recursion discarding the
      // current value.
      else filterM(t)(f)
    }
  }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = { (a: A) =>
    flatMap(f(a)) { (b: B) => g(b) }
  }

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose( (_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)((m: M[A]) => m)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

}

object Monad {

  val genMonad = new Monad[Gen] {

    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {

    override def unit[A](a: => A) = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]) =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {

    override def unit[A](a: => A) = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]) =
      p.flatMap(ma)(f)

  }

  val optionMonad: Monad[Option] = new Monad[Option] {

    override def unit[A](a: => A) = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]) =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {

    override def unit[A](a: => A) = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]) =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {

    override def unit[A](a: => A) = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]) =
      ma flatMap f
  }

  // This uses the type lambda style from the solutions.
  // Ensures we can override the flatMap method correctly since this defines an
  // argument ma with type M[A]. Without defining an explicit type we cannot
  // override the flatMap since state has two types, e.g. State[S,A].
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {

    override def unit[A](a: => A): State[S,A] = State(s => (a, s))

    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]) =
      ma.flatMap(f)
  }

  lazy val idMonad: Monad[Id] = new Monad[Id] {

    override def unit[A](a: => A) = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]) =
      ma.flatMap(f)
  }

  def readerMonad[R] = Reader.readerMonad[R]
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {

  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {

    override def unit[A](a: => A): Reader[R,A] = Reader(_ => a)

    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(st.run(r)).run(r))

  }

}

