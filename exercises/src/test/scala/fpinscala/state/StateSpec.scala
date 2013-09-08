package fpinscala.state

import org.scalatest.FunSuite

class StateSpec extends FunSuite {

  ignore("Experiments on state definitions") {
    case class State(value:Int)
    type Run[A,S] = S => (A,S)
    def unit[A,S](a: A): Run[A,S] = state => (a, state)
    def state = State(0)
    val runner = unit[Int, State](1)
    assert(runner(state) === (1, State(0)))
  }

  ignore("Trace a single state modification") {
    import State._

    case class MyState(value: Int)

    val state: State[MyState, Int] = State.unit[MyState, Int](1)

    // This line does not modifiy any state until modifiedState.run is executed, but it will
    // modify the state value to 42 later.
    // NOTE:
    println("at modifiedState")
    val modifiedState: State[MyState, Unit] = modify((s: MyState) => MyState(42))

    println("at run")
    assert(modifiedState.run(MyState(1)) === ((), MyState(42)))

    val (result, endState) = state.run(MyState(1))

    assert(result === 1)

    assert(endState === MyState(1))
  }

  test("Trace multiple state modifications as of Candy.simulateMachine") {

    import State._
    case class MyState(value: Int)
    val initialValue = 42
    val initialState = MyState(initialValue)
    def stateStubWithBusinessValue = State.unit[MyState, Int](_)
    val inputStubs: List[State[MyState, Int]] = List(stateStubWithBusinessValue(1), stateStubWithBusinessValue(2))

    // Convert list of inputs into a State. We totally ignore the integer values passed above and just concentrate
    // on the state manipulation
    // NOTE: This block is computed until the State.get calls and no further
    def sequence: State[MyState, List[Unit]] = {
      println("Step 2: in Test.sequence -- entered the mapping phase")
      val seq = State.sequence {
        println("Step 2.1: mapping state functions")
        inputStubs.map { _ =>
          println("Step 2.2: in Test.sequence at inputStubs.map before modify")
          val state: State[MyState, Unit] = modify((s: MyState) => MyState(s.value + 1))
          println(s"Step 2.4: in Test.sequence at inputStubs.map after modify, result: $state")
          state
        }
      }
      println("Step 2.6: call to Test.sequence finished.")
      seq
    }

    def prospectiveFinalState: State[MyState, Int] = {
      println("Step 1: in Test.prospectiveFinalState")
      val seq: State[MyState, List[Unit]] = sequence // Step 2. is in sequence
      println("Step 3: in Test.prospectiveFinalState: Apply state changes now (State.flatMap executes the run methods)")
      seq flatMap { _ => // Steps 3.x are in State.modify and get executed right now by flatMap
        println("Step 4: in Test.prospectiveFinalState. State changes have been applied. Get final state now")
        get map { s =>
          println(s"Step 5: in Test.prospectiveFinalState. Got final state: $s")
          s.value
        }
      }
    }

    /* The two functions above could be written as a for comprehension as the given solution for exercise 6.13 does.

    val prospectiveFinalState: State[MyState, Int] = for {
      _ <- State.sequence {
             inputStubs.map { _ =>
               modify((s: MyState) => MyState(s.value + 1))
             }
           }
      s <- get
    } yield s.value

    */

    println("Step 1: call Test.prospectiveFinalState.run(initialState)")
    assert(prospectiveFinalState.run(initialState) === (44, MyState(44)))
  }

}
