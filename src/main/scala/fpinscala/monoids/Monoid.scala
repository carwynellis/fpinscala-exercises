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

  def count(s: String): Int = {

    def wcForString(w: String) = if (w.length == 0) 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid){ c: Char =>
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    } match {
      case Stub(_) => 0
      case Part(l, n, r) =>
        wcForString(l) + n + wcForString(r)
    }

  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A,B)] {
    def op(a1: (A, B), a2: (A, B)) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    def zero = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: (A) => B, a2: (A) => B) = (v: A) => B.op(a1(v), a2(v))
    def zero = _ => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {

    def m = mapMergeMonoid[A, Int](intAddition)

    as.foldLeft(Map.empty[A, Int])( (acc, elem) => m.op(acc, Map(elem -> 1)))
  }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((elem, acc) => mb.op(f(elem), acc))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])( (elem, acc) => elem :: acc )
}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)( (acc, elem) => mb.op(acc, f(elem)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid.foldMapV

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  // We can delegate to foldMapV to take advantage of the indexed seq.
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  // There isn't a meaningful zero value for the tree so can't use foldLeft to
  // implement the foldMap. So this foldMap mirrors the structure of foldLeft,
  // differing in the application of the supplied monad op.
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(v) => f(v)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(v) => f(z, v)
    // In the branch case evaluate the left and pass that in as the z to then
    // evaluate the right.
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(v) => f(v, z)
    // In the branch case evaluate the right and pass that in as the z to then
    // evaluate the left.
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)( (acc, elem) => mb.op(acc, f(elem)))

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
}

