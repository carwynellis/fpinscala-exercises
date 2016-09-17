package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, r) if i >= 0 => (i, r)
    case (i, r) if i > Int.MinValue => (-i, r)
    case (i, r) if i == Int.MinValue => nonNegativeInt(r)
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (i, r) => (i.toDouble / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    rng.nextInt match {
      case (i: Int, r1: RNG) => double(r1) match {
        case (d: Double, r2: RNG) => ((i,d), r2)
      }
    }


  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((i: Int, d: Double), r: RNG) => ((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = double(rng) match {
    case (d1: Double, r1: RNG) => double(r1) match {
      case (d2: Double, r2: RNG) => double(r2) match {
        case (d3: Double, r3: RNG) => ((d1,d2,d3), r3)
      }
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(c: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (c == 0) (l, r)
      else {
        val (nextInt, nextRNG) = r.nextInt
        loop(c-1, nextRNG, nextInt :: l)
      }
    }
    loop(count, rng, List.empty[Int])
  }

  def doubleUsingMap(rng: RNG): Rand[Double] =
    map(nonNegativeInt){ i => i.toDouble / (Int.MaxValue.toDouble + 1) }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rngA) = ra(rng)
    val (b, rngB) = rb(rngA)
    (f(a,b), rngB)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (e, acc) =>
      map2(e, acc) { (a, b) =>
        a :: b
      }
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (fVal, fRNG) = f(rng)
    g(fVal)(fRNG)
  }

  val positiveInt: Rand[Int] = nonNegativeInt(_)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(positiveInt) { i =>
      val mod = i % n
      if ( i + (n - 1) - mod >= 0 ) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))

  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a,b)
      }
    }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a))  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b =>
        f(a,b)
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S,B]( (s: S) => {
    val (a, nextState) = run(s)
    f(a).run(nextState)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(State.unit[S, List[A]](List.empty[A])) { (e, acc) =>
      e.map2(acc) { (a, b) => a :: b }
    }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
