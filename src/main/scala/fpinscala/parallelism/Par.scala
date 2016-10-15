package fpinscala.parallelism

import java.util.concurrent._

import language.implicitConversions
import scala.concurrent.duration.Duration

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a
  // simple implementation of `Future` that just wraps a constant value. It
  // doesn't use the `ExecutorService` at all. It's always done and can't be
  // cancelled. Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  // Future implementation specific to map2 that respects timeouts
  // Derived from the provided solution which runs each future sequentially.
  private case class Map2Futures[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {

    @volatile
    private var result: Option[C] = None

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def get(): C = getResult(Long.MaxValue, TimeUnit.NANOSECONDS)

    override def get(timeout: Long, unit: TimeUnit): C =
      getResult(timeout, unit)

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = result.isDefined

    private def getResult(timeout: Long, unit: TimeUnit): C = result match {
      case Some(r) => r
      case None => run(timeout, unit)
    }

    // Runs the futures sequentially. Gives the first future the entire timeout
    // duration. The second future receives the remaining time.
    private def run(timeout: Long, unit: TimeUnit): C = {

      val timeoutNanos = Duration(timeout, unit).toNanos

      val startTime = System.nanoTime()

      val resultA = a.get(timeoutNanos, TimeUnit.NANOSECONDS)

      val stopTime = System.nanoTime()

      val remainingTime = timeoutNanos - (stopTime - startTime)

      val resultB = b.get(remainingTime, TimeUnit.NANOSECONDS)

      val res = f(resultA, resultB)

      result = Some(res)

      res
    }

  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in
  // accord with our design choice of having `fork` be the sole function in the
  // API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if
  // we want the evaluation of `f` to occur in a separate thread.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      Map2Futures(af, bf, f)
    }

  // This is the simplest and most natural implementation of `fork`, but there
  // are some problems with it--for one, the outer `Callable` will block waiting
  // for the "inner" task to complete. Since this blocking occupies a thread in
  // our thread pool, or whatever resource backs the `ExecutorService`, this
  // implies that we're losing out on some potential parallelism. Essentially,
  // we're using two threads when one should suffice. This is a symptom of a
  // more serious problem with the implementation, and we will discuss this
  // later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // Fold over the input list, ps and accumulate the elements inside a
  // Par[List[A] - this is almost identical to the Option.sequence implemented
  // earlier.
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List.empty[A])){ (elem, acc) =>
      map2(elem, acc)(_ :: _)
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in
  // the standard library. Unlike lists, these sequences provide an efficient
  // `splitAt` method for dividing them into two parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
    // `headOption` is a method defined on all collections in Scala. We saw this
    // function in chapter 3.
      ints.headOption getOrElse 0
    else {
      // Divide the sequence in half using the `splitAt` function.
      val (l,r) = ints.splitAt(ints.length/2)
      // Recursively sum both halves and add the results together.
      sum(l) + sum(r)
    }

}
