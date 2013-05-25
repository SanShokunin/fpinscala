package fpinscala.datastructures

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import fpinscala.datastructures.List._

@RunWith(classOf[JUnitRunner])
class ListSpec extends FunSuite with BeforeAndAfter {

  var list: List[Int] = _

  before {
    list = List(1,2,3,4,5)
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

  test("Compute the length of a large list") {
    intercept[java.lang.StackOverflowError] {
      val largeList = List(0 to 10000: _*)
      List.length(largeList)
    }
  }
}
