package fpinscala.errorhandling

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OptionSpec extends FunSuite with BeforeAndAfter {

  val someone: Option[Int] = Some(1)
  val none: Option[Int] = None

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
}
