package fpinscala.errorhandling

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OptionSpec extends FunSuite with BeforeAndAfter {

  val someone: Option[Int] = Some(1)
  val none: Option[Int] = None

  private def safeDivider(denom: Int)(nomin: Int): Option[Int] = if (denom == 0) None else Some(nomin/denom)
  private val safelyDivideByZero = safeDivider(0) _

  test("None.getOrElse") {
    assert(none.getOrElse(1) === 1)
  }

  test("Some.getOrElse") {
    assert(someone.getOrElse(2) === 1)
  }

  test("Some(1) should be `map`able") {
    assert(someone.map(_+1) === Some(2))
  }

  test("None should stay None when `map`ed") {
    assert(none.map(_+1) === None)
  }

  test("Some(1) `map`ed with a function that can itself fail") {
    assert(someone.map(safelyDivideByZero) === Some(None))
  }

  test("Some(1) `map`ed with an unsafe division by zero") {
    intercept[java.lang.ArithmeticException] {
      assert(someone.map(_/0) === Some(None))
    }
  }

  test("Some(1) `flatMap`ed with a function that can itself fail") {
    assert(someone.flatMap(safelyDivideByZero) === None)
  }

  /**
   * With flatMap u can/should pass "safe" operations/functions, i.e. functions
   * which itself handle errors in a functional way with Option. When using map
   * you do not have this option passing safe functions.
   *
   * However, if the caller passes a function which is not "safe" the
   * corresponding error will be propagated.
   */
  test("Some(1) `flatMap`ed with an unsafe division by zero") {
    intercept[java.lang.ArithmeticException] {
      assert(someone.flatMap((a: Int) => Some(a/0)) === None)
    }
  }

  test("Return the second Option if the first Option is undefined") {
    assert(none.orElse(Some(3)) === Some(3))
  }

  test("Return the first Option") {
    assert(someone.orElse(Some(3)) === Some(1))
  }

  test("Filter on values greater or equal 1") {
    assert(someone.filter(_ >= 1) === Some(1))
  }

  test("Filter type Char on Some(1)") {
    assert(someone.filter(_.isInstanceOf[Char]) === None)
  }

  test("Filter type Char on Some(1) with a filter function based on flatMap") {
    assert(someone.filter_1(_.isInstanceOf[Char]) === None)
  }

  ignore("Exercise 4.2") {}

  test("A generic function map2, that combines two Option values using a binary function.") {
    assert(Option.map2(someone, someone)(_+_) === Some(2))
  }

  test("A generic function map2, that combines two Option values using a binary function (2).") {
    assert(Option.map2(someone, none)(_+_) === None)
  }

  test("A generic function map2, that combines two Option values using a binary function (3).") {
    assert(Option.map2(none, someone)(_+_) === None)
  }

  test("A generic function map2, that combines two Option values using a binary function (4).") {
    assert(Option.map2(none, none)(_+_) === None)
  }
}
