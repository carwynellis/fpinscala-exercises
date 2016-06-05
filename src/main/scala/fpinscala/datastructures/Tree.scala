package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(i: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => i(v)
    case Branch(l, r) => b(fold(l)(i)(b), fold(r)(i)(b))
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((x: Int, y: Int) => 1 + x + y)

  def maximumFold(t: Tree[Int]): Int = fold(t)(i => i)((x: Int, y: Int) => x max y)

  def depthFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((x: Int, y: Int) => 1 + (x max y))
}