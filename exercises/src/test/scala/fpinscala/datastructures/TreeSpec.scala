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
}
