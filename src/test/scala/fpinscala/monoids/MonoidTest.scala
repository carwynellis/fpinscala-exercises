package fpinscala.monoids

import java.util.concurrent.Executors

import fpinscala.monoids.Monoid.{Part, Stub, WC}
import fpinscala.parallelism.Nonblocking.Par
import fpinscala.state.{RNG, State}
import fpinscala.testing.{Gen, Prop}
import org.scalatest.{FunSuite, Matchers}

class MonoidTest extends FunSuite with Matchers {

  test("stringMonoid combines two strings correctly") {
    Monoid.stringMonoid.op("foo", "bar") should be("foobar")
  }

  test("listMonoid combines two lists correctly") {
    Monoid.listMonoid.op(List(1,2), List(3,4)) should be(List(1,2,3,4))
  }

  test("intAddition monoid adds two ints correctly") {
    Monoid.intAddition.op(1, 2) should be(3)
  }

  test("intMultiplication monoid adds two ints correctly") {
    Monoid.intMultiplication.op(1, 2) should be(2)
  }

  test("booleanOr monoid combines two booleans correctly") {
    Monoid.booleanOr.op(true, false) should be(true)
  }

  test("booleanAnd monoid combines two booleans correctly") {
    Monoid.booleanAnd.op(true, false) should be(false)
  }

  test("optionMonoid combines two options correctly") {
    Monoid.optionMonoid.op(Some(1), Some(2)) should be(Some(1))
  }

  test("endoMonoid combines two endofunctions correctly") {
    val f: Int => Int = _ + 1
    val g: Int => Int = _ * 2

    val fg = Monoid.endoMonoid.op(f, g)

    fg(3) should be(8)
  }

  test("monoidLaws should hold for Option Monoid") {
    val chooseInt: Gen[Int] = Gen.choose(0, Int.MaxValue)
    val optionMonoidLaws = Monoid.monoidLaws[Option[Int]](
      Monoid.optionMonoid[Int],
      chooseInt.map(Some(_))
    )
    // TODO - run returns unit but generates output to STDOUT
    Prop.run(optionMonoidLaws)
  }

  test("concenate folds a list with a monoid correctly") {
    val data = List("foo", "bar", "baz")
    Monoid.concatenate(data, Monoid.stringMonoid) should be("foobarbaz")
  }

  test("foldMap folds a list correctly") {
    val data = List("foo", "bar", "baz")
    Monoid.foldMap(data, Monoid.stringMonoid)(identity) should be("foobarbaz")
  }

  test("foldLeft should fold a list correctly") {
    val data = List("foo", "bar", "baz")
    Monoid.foldLeft(data)(""){ (acc: String, e: String) =>
      Monoid.stringMonoid.op(e, acc)
    } should be("foobarbaz")
  }

  test("foldRight should fold a list correctly") {
    val data = List("foo", "bar", "baz")
    Monoid.foldRight(data)(""){ (e: String, acc: String) =>
      Monoid.stringMonoid.op(e, acc)
    } should be("bazbarfoo")
  }

  test("foldMapV should fold an indexedSeq with an even number of elements correctly") {
    val data = IndexedSeq("a", "b", "c", "d")
    Monoid.foldMapV(data, Monoid.stringMonoid)(identity) should be("abcd")
  }

  test("foldMapV should fold an indexedSeq with an odd number of elements correctly") {
    val data = IndexedSeq("a", "b", "c", "d", "e")
    Monoid.foldMapV(data, Monoid.stringMonoid)(identity) should be("abcde")
  }

  test("parFoldMap should fold an indexedSeq correctly") {
    val data = IndexedSeq("a", "b", "c", "d")
    val executorService = Executors.newWorkStealingPool()

    val parMonoid = Monoid.parFoldMap(data, Monoid.stringMonoid)(identity)

    Par.run(executorService)(parMonoid) should be("abcd")
  }

  test("ordered returns true for an ordered indexedSeq") {
    val ordered = IndexedSeq(1,2,3,4,5)
    Monoid.ordered(ordered) should be(true)
  }

  test("ordered returns false for an unordered indexedSeq") {
    val unordered = IndexedSeq(5,4,3,2,1)
    Monoid.ordered(unordered) should be(false)
  }

  test("ordered returns false for another unordered indexedSeq") {
    val unordered = IndexedSeq(1,2,3,5,4)
    Monoid.ordered(unordered) should be(false)
  }

  test("ordered returns true for an indexedSeq containing identical values") {
    val ordered = IndexedSeq(1,1,1,1,1)
    Monoid.ordered(ordered) should be(true)
  }

  test("monoidLaws should hold for WC Monoid") {
    def chooseWC: Gen[WC] = {
      val state = State(RNG.nonNegativeInt) map { n =>
        if (n % 2 == 1) {
          // TODO - gen string randomly
          Stub("a")
        }
        else {
          // TODO - gen strings
          Part("b", n, "c")
        }
      }
      Gen(state)
    }
    val wcMonoidLaws = Monoid.monoidLaws[WC](
      Monoid.wcMonoid,
      chooseWC
    )
    // TODO - run returns unit but generates output to STDOUT
    Prop.run(wcMonoidLaws)
  }

  test("count returns the correct word count for a given string") {
    val text = "lorem ipsum dolor sit amet"
    Monoid.count(text) should be(5)
  }

  test("count returns 0 for empty string") {
    Monoid.count("") should be(0)
  }

  test("count returns 0 for a string containing only spaces") {
    Monoid.count("          ") should be(0)
  }

  test("count ignores leading and trailing whitespace") {
    val text = "   lorem ipsum dolor sit amet    "
    Monoid.count(text) should be(5)
  }

  test("ListFoldable foldRight returns expected result") {
    val data = List(1, 2, 3, 4, 5)
    val result: String = ListFoldable.foldRight(data)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("54321")
  }

  test("ListFoldable foldLeft returns expected result") {
    val data = List(1, 2, 3, 4, 5)
    val result: String = ListFoldable.foldLeft(data)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("12345")
  }

  test("ListFoldable foldMap returns expected result") {
    val data = List(1, 2, 3, 4, 5)
    val result: String = ListFoldable.foldMap[Int, String](data) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("23456")
  }

  test("IndexedSeqFoldable foldRight returns expected result") {
    val data = IndexedSeq(1, 2, 3, 4, 5)
    val result: String = IndexedSeqFoldable.foldRight(data)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("54321")
  }

  test("IndexedSeqFoldable foldLeft returns expected result") {
    val data = IndexedSeq(1, 2, 3, 4, 5)
    val result: String = IndexedSeqFoldable.foldLeft(data)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("12345")
  }

  test("IndexedSeqFoldable foldMap returns expected result") {
    val data = IndexedSeq(1, 2, 3, 4, 5)
    val result: String = IndexedSeqFoldable.foldMap[Int, String](data) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("23456")
  }

  test("StreamFoldable foldRight returns expected result") {
    val data = Stream(1, 2, 3, 4, 5)
    val result: String = StreamFoldable.foldRight(data)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("54321")
  }

  test("StreamFoldable foldLeft returns expected result") {
    val data = Stream(1, 2, 3, 4, 5)
    val result: String = StreamFoldable.foldLeft(data)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("12345")
  }

  test("StreamFoldable foldMap returns expected result") {
    val data = Stream(1, 2, 3, 4, 5)
    val result: String = StreamFoldable.foldMap[Int, String](data) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("23456")
  }

}

