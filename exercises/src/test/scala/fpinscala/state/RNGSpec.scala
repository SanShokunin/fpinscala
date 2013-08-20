package fpinscala.state

import fpinscala.state.RNG._
import org.scalatest.{FunSuite, BeforeAndAfter}

class RNGSpec extends FunSuite with BeforeAndAfter {

  val rng = RNG.simple(System.currentTimeMillis)

  test("Combine a list of transitions into a single transition") {
    assert(sequence(List(unit(1), unit(2), unit(3)))(rng)._1  === List(1, 2, 3))
  }

}
