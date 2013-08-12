package fpinscala.errorhandling

import org.scalatest.{FunSuite, BeforeAndAfter}

class EitherSpec extends FunSuite with BeforeAndAfter {

  test("A for comprehension with a Left value returns that Left value") {
    val result = for {
      age <- Right(42)
      name <- Left("invalid name")
      salary <- Right(1000000.0)
    } yield (name, age, salary)
    assert(result === Left("invalid name"))
  }

  test("A flatMap with a nested Left value returns that Left value") {
    val result =
      Right(42) flatMap { age =>
        Left("invalid name") map { name =>
          (age, name, Right(100000.0))
        }
      }
    assert(result === Left("invalid name"))
  }
}
