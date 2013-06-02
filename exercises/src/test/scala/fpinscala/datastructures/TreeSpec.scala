package fpinscala.datastructures

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import fpinscala.datastructures.Tree._

@RunWith(classOf[JUnitRunner])
class TreeSpec extends FunSuite with BeforeAndAfter {

  var tree: Tree[Int] = _

  before {
    tree = Branch(Leaf(1),Branch(Leaf(2), Leaf(3)))
  }

  test("Count the number of nodes in a tree") {
    assert(Tree.size(tree) === 5)
  }

  test("Return the maximum element of a tree") {
    assert(Tree.maximum(tree) === 3)
  }

  test("Compute the depth of a tree") {
    assert(Tree.depth(tree) === 2)
  }

  test("Compute the depth of an empty tree") {
    assert(Tree.depth(Leaf(1)) === 1)
  }

  test("Map values of a tree") {
    assert(Tree.map(tree)(_*2) === Branch(Leaf(2),Branch(Leaf(4), Leaf(6))))
  }

  test("Compute the number of nodes in a tree in terms of fold") {
    assert(Tree.size2(tree) === 5)
  }

  test("Return the maximum element of a tree in terms of fold") {
    assert(Tree.maximum2(tree) === 3)
  }

  test("Compute the depth of a tree in terms of fold") {
    assert(Tree.depth2(tree) === 2)
  }

  test("Compute the depth of an empty tree in terms of fold") {
    assert(Tree.depth2(Leaf(1)) === 1)
  }

  test("Map values of a tree in terms of fold") {
    assert(Tree.map2(tree)(_*2) === Branch(Leaf(2),Branch(Leaf(4), Leaf(6))))
  }
}
