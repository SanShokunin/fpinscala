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

  def size2[A](t: Tree[A]): Int = fold(t)(a => 1) { (l,r) =>
    1 + size2(l) + size2(r)
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

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a){ (l,r) =>
    maximum2(l) max maximum2(r)
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

  def depth2[A](t: Tree[A]): Int = fold(t)(a => 1) { (l,r) =>
    1 + depth2(l) max depth2(r)
  }

  /**
   * Exercise 3.28
   *
   * Write a function map, analogous to the method of the same name on List,
   * that modifies each element in a tree with a given function.
   */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B]) { (l,r) =>
    Branch(map2(l)(f), map2(r)(f))
  }

  /**
   * Exercise 3.29
   *
   * Generalize size, maximum, depth, and map, writing a new function fold that
   * abstracts over their similarities. Reimplement them in terms of this more
   * general function. Can you draw an analogy between this fold function and
   * the left and right folds for List?
   */
  def fold[A,B](t: Tree[A])(z: A => B)(g: (Tree[A], Tree[A]) => B): B = t match {
    case Leaf(a) => z(a)
    case Branch(l,r) => g(l,r)
  }

}