package fpinscala.errorhandling

import org.scalatest.{FunSuite, BeforeAndAfter}

class EitherSpec extends FunSuite with BeforeAndAfter {

  val listWithRights = List(Right(1), Right(2), Right(3))

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

  test("A list List[Either[E,A]] should be transformed into a Either[E, List[A]]") {
    assert(Either.sequence(listWithRights) === Right(List(1,2,3)))
  }

  test("A list List[Either[E,A]] should be transformed into a Either[E, List[A]] using foldRight under the hood") {
    assert(Either.sequence_1(listWithRights) === Right(List(1,2,3)))
  }

}
