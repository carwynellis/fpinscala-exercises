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
}
