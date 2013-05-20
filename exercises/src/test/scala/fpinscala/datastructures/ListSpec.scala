package fpinscala.datastructures

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import fpinscala.datastructures.List._

@RunWith(classOf[JUnitRunner])
class ListSpec extends FunSuite {

  trait withList {
    import fpinscala.datastructures.List
    val list = List(1,2,3,4,5)
  }

  test("Pattern matches (exercise 3.1)") {
    new withList {
      val x = list match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      assert(x === 3)
    }
  }

  test("Tail of a non-empty list") {
    new withList {
      assert(List.tail(list) === List(2,3,4,5))
    }
  }

  test("Tail of an empty list") {
    new withList {
      assert(List.tail(List()) === Nil)
    }
  }

  test("Drop 5 elements from a list") {
    new withList {
      assert(List.drop(list, 5) === Nil)
    }
  }

  test("Drop 1 element from a list") {
    new withList {
      assert(List.drop(list, 1) === List(2,3,4,5))
    }
  }

  test("Drop element from an empty list") {
    assert(List.drop(Nil, 1) == Nil)
  }

}
