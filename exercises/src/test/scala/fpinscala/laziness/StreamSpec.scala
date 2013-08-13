package fpinscala.laziness

import org.scalatest.{BeforeAndAfter, FunSuite}

class StreamSpec extends FunSuite with BeforeAndAfter {

  val stream = Stream.cons(1, Stream(2))

  test("A stream should be converted into a list") {
    assert(stream.toList === List(1,2))
  }

}
