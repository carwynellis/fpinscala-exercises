package fpinscala.datastructures

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("size should return 1 for a tree composed of a single node") {
    val t = Leaf(1)
    assertResult(1) { Tree.size(t) }
  }

  test("size should return three for a tree composed of a single branch with two leaf nodes") {
    val t = Branch(Leaf(1), Leaf(2))
    assertResult(3) { Tree.size(t) }
  }

  test("maximum should return the only value of a single element tree") {
    val t = Leaf(1)
    assertResult(1) { Tree.maximum(t) }
  }

  test("maximum should return the maximum of a three node tree") {
    val t = Branch(Leaf(1), Leaf(2))
    assertResult(2) { Tree.maximum(t) }
  }

  test("depth returns 0 for a tree composed of a single leaf") {
    val t = Leaf(0)
    assertResult(0) { Tree.depth(t) }
  }

  test("depth returns 1 for a tree with single branch with two leaf nodes") {
    val t = Branch(Leaf(1), Leaf(2))
    assertResult(1) { Tree.depth(t) }
  }

  test("depth returns 3 for an asymmetric tree") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    assertResult(3) { Tree.depth(t) }
  }

  test("map of a single leaf tree returns single leaf tree with function applied") {
    val t = Leaf(1)
    assertResult(Leaf(2)) { Tree.map(t)((i: Int) => i + 1) }
  }

  test("map of a more complex tree returns the same structure with modified values") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    assertResult(Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))) {
      Tree.map(t)((i: Int) => i + 1)
    }
  }

  test("fold returns expected result when applied over a simple tree") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    assertResult(6) { Tree.fold(t)(i => i)((x: Int, y: Int) => x + y) }
  }

  test("sizeFold returns the number of nodes in a tree correctly") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    assertResult(7) { Tree.sizeFold(t) }
  }

  test("maximumFold returns the maximum of a tree of ints") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    assertResult(4) { Tree.maximumFold(t) }
  }

  test("depthFold returns the correct depth of a given tree") {
    val t = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    assertResult(3) { Tree.depthFold(t) }
  }
}
