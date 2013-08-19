package fpinscala.laziness

import org.scalatest.{BeforeAndAfter, FunSuite}

class StreamSpec extends FunSuite with BeforeAndAfter {

  val stream = Stream(1,2,3)
  val emptyStream = Stream.empty

  test("A stream should be converted into a list") {
    assert(stream.toList === List(1,2,3))
  }

  test("Take the first n elements of a stream") {
    assert(stream.take(2).toList === List(1,2))
  }

  test("Take the first n elements of an empty stream") {
    assert(emptyStream.take(2).toList === Nil)
  }

  test("Take 4 elements of a stream with 3 elements") {
    assert(stream.take(4).toList === List(1,2,3))
  }

  test("Take while predicate matches") {
    assert(stream.takeWhile(_ <= 2).toList === List(1,2))
  }

  test("Take while non of the elements within the stream match the predicate") {
    assert(stream.takeWhile(_ == 0).toList === Nil)
  }

  test("The given predicate does not match for all elements in the Stream") {
    assert(stream.forAll(_ % 2 == 0) === false)
  }

  test("The given predicate does match for elements in the Stream") {
    assert(stream.forAll(_ < 10) === true)
  }

  test("Take while predicate matches with takeWhile implemented in terms of foldRight") {
    assert(stream.takeWhile(_ <= 2).toList === List(1,2))
  }

  test("Take while non of the elements within the stream match the predicate with takeWhile implemented in terms of foldRight") {
    assert(stream.takeWhile(_ == 0).toList === Nil)
  }

  test("Map values of a Stream") {
    assert(stream.map(_ + 10).toList === List(11,12,13))
  }

  test("Filter values of a Stream") {
    assert(stream.filter(_ % 2 == 0).toList === List(2))
  }

  test("Map and filter values of a Stream") {
    assert(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList === List(12,14))
  }

  test("Append to a Stream") {
    assert(stream.append(stream).toList === List(1,2,3,1,2,3))
  }

  test("FlatMap a Stream") {
    assert(stream.flatMap(e => Stream(e + 10)).toList === List(11,12,13))
  }

  test("FlatMap an empty Stream") {
    assert(emptyStream.flatMap(e => Stream(e)).toList === Nil)
  }

  test("FlatMap a Stream (with given solution)") {
    assert(stream.flatMap_1(e => Stream(e + 10)).toList === List(11,12,13))
  }

  test("Map (via unfold) values of a Stream") {
    assert(stream.mapViaUnfold(_ + 10).toList === List(11,12,13))
  }

  test("Take (via unfold) the first n elements of a stream") {
    assert(stream.takeViaUnfold(2).toList === List(1,2))
  }

  test("Take (via unfold) the first n elements of an empty stream") {
    assert(emptyStream.takeViaUnfold(2).toList === Nil)
  }

  test("Take (via unfold) 4 elements of a stream with 3 elements") {
    assert(stream.takeViaUnfold(4).toList === List(1,2,3))
  }

  test("Take while predicate matches with takeWhile implemented in terms of unfold") {
    assert(stream.takeWhileViaUnfold(_ <= 2).toList === List(1,2))
  }

  ignore("Zip Streams via unfold") {
    // TODO
  }

  ignore("Zip Streams (via unfold) as long as either stream has more elements") {
    // TODO
  }

  ignore("Check if one Stream is a prefix of another") {
    // TODO
  }

  ignore("For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream") {
    // TODO assert(stream.tails === Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty))
  }

  ignore("Stream.tails in terms of scanRight") {
    // TODO assert(stream.tailsViaScanRight === Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty))
  }
}
