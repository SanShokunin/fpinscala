package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /**
   * Exercise 3.25
   *
   * Write a function size that counts the number of nodes in a tree.
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /**
   * Exercise 3.26
   *
   * Write a function maximum that returns the maximum element in a Tree[Int].
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
   * Exercise 3.27
   *
   * Write a function depth that returns the maximum path length from the root
   * of a tree to any leaf.
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }
}