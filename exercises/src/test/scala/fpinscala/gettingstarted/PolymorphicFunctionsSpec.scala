package fpinscala.gettingstarted

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * A test suite for the second exercise.
 *
 * The goal is to implement a polymorphic function `isSorted`, which tests if a
 * given `Array[A]` is sorted by using a given test function.
 *
 */
@RunWith(classOf[JUnitRunner])
class PolymorphicFunctionsSpec extends FunSuite {

  import PolymorphicFunctions.isSorted

  test("Array of Int is not sorted") {
    assert(isSorted(Array(1,2,4,3), (a: Int, b: Int) => a < b) === false)
  }

  test("Array of String is sorted") {
    assert(isSorted(Array("eins", "zwei"), (a: String, b: String) => a < b) === true)
  }

  test("Concrete usage of a polymorphic function") {
    import PolymorphicFunctions.partial1
    val multipliedByFiveAsString = partial1(5, (a: Int, b: Double) => s"${a * b}")
    assert(multipliedByFiveAsString(10.0) === "50.0")
  }
}