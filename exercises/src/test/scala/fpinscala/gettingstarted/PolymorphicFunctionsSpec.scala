package fpinscala.gettingstarted

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fpinscala.gettingstarted.PolymorphicFunctions._

/**
 * A test suite for polymorphic functions exercises.
 */
@RunWith(classOf[JUnitRunner])
class PolymorphicFunctionsSpec extends FunSuite {

  /**
   * The goal is to implement a polymorphic function `isSorted`, which tests if a
   * given `Array[A]` is sorted by using a given test function.
   */
  import PolymorphicFunctions.{isSorted, partial1, curry, uncurry}

  test("Array of Int is not sorted") {
    assert(isSorted(Array(1,2,4,3), (a: Int, b: Int) => a < b) === false)
  }

  test("Array of String is sorted") {
    assert(isSorted(Array("eins", "zwei"), (a: String, b: String) => a < b) === true)
  }

  /**
   * Tests on partial application of functions. This exercise shows the constrained
   * nature (by their type) of implementing such functions. This is because the
   * authors of such functions do not know what type exactly will be passed to it
   * in advance, thus they are somewhat limited in calling methods on it, etc.
   */
  test("Concrete usage of the polymorphic function partial1") {
    val multipliedByFiveAsString = partial1(5, (a: Int, b: Double) => s"${a * b}")
    assert(multipliedByFiveAsString(10.0) === "50.0")
  }

  test("Concrete usage of the polymorphic function curry") {
    val multipliedByFiveAsString = curry((a: Int, b: Double) => s"${a * b}")(5)
    assert(multipliedByFiveAsString(10.0) === "50.0")
  }

  test("Uncurried polymorphic function") {
    val curried = curry((a: Int, b: Double) => s"${a * b}")
    assert(uncurry(curried)(5, 10.0) == "50.0")
  }
}