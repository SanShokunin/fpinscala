package fpinscala.datastructures

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import fpinscala.datastructures.List._
import scala.annotation.tailrec

@RunWith(classOf[JUnitRunner])
class ListSpec extends FunSuite with BeforeAndAfter {

  var list: List[Int] = _

  before {
    list = List(1,2,3,4,5)
  }

  /**
   * This functions helps on creation of large lists.
   *
   * The creation of large lists, e.g. by List(1 to 10000: _*), i.e. calling
   * fpinscala.datastructure.List.apply() would cause a StackOverflowError, since
   * the implementation of apply not tail-recursive.
   *
   * @param n
   * @return a list of n integers
   */
  private def createIntList(n: Int): List[Int] = {
    @tailrec
    def go(l: List[Int], n: Int): List[Int] = {
      if (n == 0) l
      else go(List.append(Cons(n, Nil), l), n - 1)
    }
    go(Nil, n)
  }

  test("Pattern matches (exercise 3.1)") {
    val x = list match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(x === 3)
  }

  test("Tail of a non-empty list") {
    assert(List.tail(list) === List(2,3,4,5))
  }

  test("Tail of an empty list") {
    assert(List.tail(List()) === Nil)
  }

  test("Drop 5 elements from a list") {
    assert(List.drop(list, 5) === Nil)
  }

  test("Drop 1 element from a list") {
    assert(List.drop(list, 1) === List(2,3,4,5))
  }

  test("Drop element from an empty list") {
    assert(List.drop(Nil, 1) == Nil)
  }

  test("dropWhile numbers are odd") {
    val drop = List.dropWhile(list) _
    assert(drop(x => (x % 2 != 0)) === List(2,3,4,5))
  }

  test("dropWhile numbers are even") {
    val drop = List.dropWhile(List(2,4,6,9,11)) _
    assert(drop(x => (x % 2 == 0)) === List(9, 11))
  }

  test("dropWhile on empty list") {
    val drop = List.dropWhile(List()) _
    assert(drop(x => true) === Nil)
  }

  test("Replace the first element of a non-empty list") {
    assert(List.setHead(list)(42) === List(42,2,3,4,5))
  }

  test("Replace the first element of an empty list") {
    assert(List.setHead(Nil)(42) === Nil)
  }

  test("Return all but the last element of a list") {
    assert(List.init(list) === List(1,2,3,4))
  }

  test("Compute the length of a list") {
    assert(List.length(list) === 5)
  }

  test("Compute the length of an empty list") {
    assert(List.length(Nil) === 0)
  }

  test("Compute the length of a large list using the not tail-recursive function foldRight behind the scene") {
    intercept[java.lang.StackOverflowError] {
      List.length(createIntList(100000))
    }
  }

  test("Compute the length of a list using (tail-recursive) foldLeft") {
    assert(List.foldLeft(createIntList(100000), 0)((acc, _) => acc + 1) === 100000)
  }
}
