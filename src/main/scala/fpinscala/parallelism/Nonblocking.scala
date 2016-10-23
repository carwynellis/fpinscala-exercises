package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Future}
import java.util.concurrent.atomic.AtomicReference

import language.implicitConversions

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(
      k: A => Unit,
      // Error handler with rudimentary default implementation
      eh: Exception => Unit = e => println(s"Caught exception: $e")
    ): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      // A mutable, threadsafe reference, to use for storing the result
      val ref = new AtomicReference[Either[Exception, A]]
      // A latch which, when decremented, implies that `ref` has the result
      val latch = new CountDownLatch(1)

      // Asynchronously set the result, and decrement the latch.
      p(es)(
        { result =>
        // When an exception is raised we won't even get here so block forever
        ref.set(Right(result))
        latch.countDown()
      },
        { e: Exception =>
          ref.set(Left(e))
          latch.countDown()
        }
      )

      // Block until the `latch.countDown` is invoked asynchronously
      latch.await()
      // TODO - the IOMonad code depends on this implementation returning an
      //        A so for now match on the either and return the value or throw
      //        an exception. Would prefer to leave it to the caller to decide
      //        whether an exception needed to be thrown.
      ref.get match {
        case Left(e)    => throw e
        case Right(value) => value
      }
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit = {
          cb(a)
        }
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit, eh: Exception => Unit) = f(k)
    }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })


    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit, eh: Exception => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A,B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        // cb is the function passed to the par in the run method
        def apply(cb: B => Unit, eh: Exception => Unit): Unit = {
          // We eval here so at this point we should start another thread
          p(es)(a => eval(es) {
            // Call the function and pass the result to the callback if no
            // exceptions were thrown, otherwise pass the caught exception
            // to the error handling callback.
            try {
              val result = f(a)
              cb(result)
            }
            catch {
              case e: Exception => eh(e)
            }
          })
        }
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN( p map {c => if (c) 0 else 1 })(List(t,f))

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit =
          p(es) { c => eval(es) { ps(c)(es)(cb) } }
      }

    def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
      es => new Future[V] {
        def apply(cb: V => Unit, eh: Exception => Unit): Unit =
          p(es) { key =>
            eval(es) {
              ps.get(key) foreach { chosenPar =>
                chosenPar(es)(cb)
              }
            }
          }
      }

    def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit, eh: Exception => Unit): Unit =
          p(es) { c =>
            eval(es) {
              f(c)(es)(cb)
            }
          }
      }

    def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      chooser(p) { c =>
        if (c) t
        else f
      }

    def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(p) { c => choices(c) }

    def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] = chooser(p)(f)

    def join[A](p: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit =
          p(es) { inner: Par[A] => inner(es)(cb) }
      }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(r => r)

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = join(p.map(f))

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }
  }
}
