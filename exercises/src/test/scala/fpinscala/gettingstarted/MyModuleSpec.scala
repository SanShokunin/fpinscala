package fpinscala.gettingstarted

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * A test suite for the first exercise.
 *
 * In this exercise a function to compute the nth Fibonacci has to be
 * implemented.
 */
@RunWith(classOf[JUnitRunner])
class MyModuleSpec extends FunSuite {

  import MyModule.fib

  test("First Fibonacci number") {
    assert(fib(0) === 0)
  }

  test("7th Fibonacci number") {
      assert(fib(7) === 13)
    }

  test("20th Fibonacci number") {
    assert(fib(20) === 6765)
  }
}
