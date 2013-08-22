package fpinscala.state

import org.scalatest.FunSuite

class CandySpec extends FunSuite {

  test("if the input Machine has 10 coins in it, and a net total of 4 coins are added in the inputs, the output will be 14.") {
    val machine = Machine(true, 4, 10)
    val sim = Candy.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    assert(sim.run(machine) === (14, Machine(true, 0, 14)))
  }

}
