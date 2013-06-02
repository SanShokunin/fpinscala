package fpinscala.errorhandling

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import fpinscala.errorhandling.{Option,Some,None}

@RunWith(classOf[JUnitRunner])
class OptionSpec extends FunSuite with BeforeAndAfter {

  test("None.getOrElse") {
    assert(None.getOrElse(1) === 1)
  }

  test("Some.getOrElse") {
    assert(Some(1).getOrElse(2) === 1)
  }
}
