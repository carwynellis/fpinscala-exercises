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
    val genWC: Gen[Monoid.WC] = Gen.choose(1, 10).map2(Gen.string(10)) { (i, s) =>
      if (i % 2 == 0) Stub(s)
      else Part(s, i, s)
    }

    val wcMonoidLaws = Monoid.monoidLaws[WC](
      Monoid.wcMonoid,
      genWC
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
    val l = List(1, 2, 3, 4, 5)
    val result: String = ListFoldable.foldRight(l)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("54321")
  }

  test("ListFoldable foldLeft returns expected result") {
    val l = List(1, 2, 3, 4, 5)
    val result: String = ListFoldable.foldLeft(l)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("12345")
  }

  test("ListFoldable foldMap returns expected result") {
    val l = List(1, 2, 3, 4, 5)
    val result: String = ListFoldable.foldMap[Int, String](l) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("23456")
  }

  test("IndexedSeqFoldable foldRight returns expected result") {
    val i = IndexedSeq(1, 2, 3, 4, 5)
    val result: String = IndexedSeqFoldable.foldRight(i)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("54321")
  }

  test("IndexedSeqFoldable foldLeft returns expected result") {
    val i = IndexedSeq(1, 2, 3, 4, 5)
    val result: String = IndexedSeqFoldable.foldLeft(i)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("12345")
  }

  test("IndexedSeqFoldable foldMap returns expected result") {
    val i = IndexedSeq(1, 2, 3, 4, 5)
    val result: String = IndexedSeqFoldable.foldMap[Int, String](i) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("23456")
  }

  test("StreamFoldable foldRight returns expected result") {
    val s = Stream(1, 2, 3, 4, 5)
    val result: String = StreamFoldable.foldRight(s)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("54321")
  }

  test("StreamFoldable foldLeft returns expected result") {
    val s = Stream(1, 2, 3, 4, 5)
    val result: String = StreamFoldable.foldLeft(s)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("12345")
  }

  test("StreamFoldable foldMap returns expected result") {
    val s = Stream(1, 2, 3, 4, 5)
    val result: String = StreamFoldable.foldMap[Int, String](s) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("23456")
  }

  test("TreeFoldable foldRight returns expected result") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val result: String = TreeFoldable.foldRight(t)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("321")
  }

  test("TreeFoldable foldLeft returns expected result") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val result: String = TreeFoldable.foldLeft[Int, String](t)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("123")
  }

  test("TreeFoldable foldMap returns expected result") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val result: String = TreeFoldable.foldMap[Int, String](t) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("234")
  }

  test("OptionFoldable foldRight returns expected result") {
    val o = Some(5)
    val result: String = OptionFoldable.foldRight(o)(""){ (elem, acc) =>
      acc + s"$elem"
    }
    result should be("5")
  }

  test("OptionFoldable foldLeft returns expected result") {
    val o = Some(5)
    val result: String = OptionFoldable.foldLeft(o)(""){ (acc, elem) =>
      acc + s"$elem"
    }
    result should be("5")
  }

  test("OptionFoldable foldMap returns expected result") {
    val o = Some(5)
    val result: String = OptionFoldable.foldMap[Int, String](o) { (i: Int) =>
      (i + 1).toString
    } (Monoid.stringMonoid)
    result should be("6")
  }

  test("OptionFoldable toList returns a single element list for Some value") {
    val o = Some(5)
    OptionFoldable.toList(o) should be(List(5))
  }

  test("OptionFoldable toList returns an empty list for None") {
    val o = None
    OptionFoldable.toList(o) should be(List.empty)
  }

  test("StreamFoldable toList returns the expected list") {
    val s = Stream(1,2,3,4,5)
    StreamFoldable.toList(s) should be(List(1,2,3,4,5))
  }

  test("TreeFoldable toList returns a list") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    TreeFoldable.toList(t) should be(List(1,2,3))
  }

  test("Product monoid composes a string and intadd monoid correctly") {
    val pm = Monoid.productMonoid(Monoid.stringMonoid, Monoid.intAddition)
    pm.op(("foo", 1), ("bar", 2)) should be("foobar", 3)
  }

  test("Function monoid combines two functions for a given monoid") {
    val fm = Monoid.functionMonoid[Int, Int](Monoid.intAddition)
    val fmFunc = fm.op(i => i + 4, i => i + 10)(2) should be(18)
  }

  test("bag computes the expected 'bag' from an indexedSeq") {
    val i = IndexedSeq("a", "rose", "is", "a", "rose")
    Monoid.bag(i) should be(Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }

}

