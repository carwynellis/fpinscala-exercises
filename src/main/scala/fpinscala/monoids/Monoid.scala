package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  lazy val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  lazy val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  lazy val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    // Note - this could be implemented as a2 orElse a1  too.
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A]  = new Monoid[A => A] {
    // Note - this could be implemented as a2 andThen a1 or a1 compose a2
    def op(a1: A => A, a2: A => A) = a1 andThen a2
    val zero = (i: A) => i
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    // Identity
    // Zero value such that op(x, zero) == op(zero, x) == x
    // for any X: A
    forAll(gen, "monoid identity law") { x: A =>
      (m.op(x, m.zero) == x) && (m.op(m.zero, x) == x)
    } &&
    // Associativity
    // Given x,y,z then op(op(x,y),z)) == op(x, op(y,z))
    forAll(
      // Generate three A values
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield(x,y,z), "monoid associativity law") { t =>
      val (x,y,z) = t
      m.op(m.op(x,y), z) == m.op(x, m.op(y,z))
    }
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (acc, a) =>
      m.op(acc, f(a))
    }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    // An empty list should be handled as a no-op or 'zero' value as per the
    // monoid being used.
    if (as.isEmpty) m.zero
    // Apply the function to a single value if that's all we have.
    else if (as.length == 1) f(as.head)
    // Otherwise continue to recurse.
    else {
      // Split the sequence into two lists of equal length, in the even case,
      // or length / 2 and (length / 2) + 1 in the odd case, and recurse down
      // until we reach a single element, whereupon the function can be
      // applied to that value and then combined with other results as we
      // return back up the stack.
      // Note that the code is somewhat more concise than the description!
      val (a, b) = as.splitAt(as.length / 2)

      m.op(foldMapV(a,m)(f), foldMapV(b,m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Define a monoid that can be used to compare two values and return a
    // boolean indicating whether the values are ordered or not, assuming
    // ascending order is required.
    // We will need some sort of tuple since the monoid types must be
    // consistent preventing us from defining and op that takes two ints and
    // returns a boolean.
    // Our life is made slightly easier since ordered is constrained to ints.
    def orderedIntMonoid: Monoid[Option[(Int, Boolean)]] = new Monoid[Option[(Int, Boolean)]] {
      def op(a: Option[(Int, Boolean)], b: Option[(Int, Boolean)]) = {
        (a,b) match {
          case (Some(l),Some(r)) =>
            // We will store the maximum value in the result
            val max = l._1 max r._1
            // If r contains the max then these two elements are sorted so we
            // can return true
            // However we must never lose a false value so combine the booleans
            // we have already with the equality test.
            val isOrdered = l._2 && r._2 && (r._1 == max)
            Some(max, isOrdered)
          // In the cases were we only have a left or right value just return
          // that since no further comparisons can be made.
          case (Some(l), None) => Some(l)
          case (None, Some(r)) => Some(r)
          // For now return the zero value
          case (None, None) => zero
        }
      }
      // TODO - it might be more correct to define this as
      //        Some(Int.MinValue, true) when combining with other values.
      def zero = None
    }

    val result = foldMapV(ints, orderedIntMonoid)(i => Some(i, true))

    result match {
      case Some((i, ordered)) => ordered
      // TODO - is this a sensible default in the None case?
      case None => false
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(x: Par[A], y: Par[A]) = x.map2(y)(m.op)
    def zero = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {

    // Map over the input seq in parallel and then fold in parallel using the
    // monoid op.
    Par.parMap(v)(f).flatMap { seq =>
      foldMapV(seq, par(m)) { e => Par.unit(e) }
    }

  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(x: WC, y: WC) = (x,y) match {
      case (Stub(lChars), Stub(rChars)) => Stub(lChars + rChars)
      case (Part(lStub, words, rStub), Stub(chars)) => Part(lStub, words, rStub + chars)
      case (Stub(chars), Part(lStub, words, rStub)) => Part(chars + lStub, words, rStub)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        // Note we need to handle case where we join two parts that may hold
        // parts of a complete word, e.g. rStub1 + lStub2 may be a word.
        val combinedWord = rStub1 + lStub2
        val combinedWordCount = if (combinedWord.isEmpty) 0 else 1
        // Now that we have combined rStub1 and lStub2 we create a new part
        // that starts with lStub1 and ends with rStub2.
        Part(lStub1, words1 + words2 + combinedWordCount, rStub2)
    }
    def zero = Stub("")
  }

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

