package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /**
   * Exercise 5.2:
   *
   * Write a function take for returning the first n elements of a
   * Stream.
   *
   * @param n Number of elements to take from this streams' head
   * @return Stream of n or less elements or even empty Stream
   */
  def take(n: Int): Stream[A] = uncons match {
    case Some((h, t)) if n > 0 => cons(h, t.take(n-1))
    case _ => empty
  }

  /**
   * Exercise 5.3:
   *
   * Write the function takeWhile for returning all starting elements of a
   * Stream that match the given predicate.
   *
   * @param p Predicate function
   * @return Stream of n or less elements or even an empty Stream
   */
  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((h, t)) if p(h) => cons(h, t.takeWhile(p))
    case _ => empty
  }

  /**
   * Exercise 5.5:
   *
   * Use foldRight to implement takeWhile. This will construct a stream
   * incrementally, and only if the values in the result are demanded by some
   * other expression.
   *
   * The foldRight's recursion does not continue since the second parameter t of
   * our function passed to foldRight, will not be evaluated until p(h) is true.
   * The parameter t in fact stays at expression t.foldRight(z)(f) (see foldRight
   * function definition above) unevaluated. The parameter h is evaluated only
   * because it's needed for the test expression.
   *
   * @param p Predicate function
   * @return Stream of n or less elements or even an empty Stream
   */
  def takeWhile_1(p: A => Boolean): Stream[A] = foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  /**
   * Exercise 5.4:
   *
   * Implement forAll, which checks that all elements in the Stream match a
   * given predicate. Your implementation should terminate the traversal as
   * soon as it encounters a non-matching value.
   *
   * @param p The predicate function
   * @return true if all elements in the Stream match the given predicate
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
   * Exercise 5.1:
   *
   * Write a function to convert a Stream to a List, which will
   * force its evaluation and let us look at it in the REPL. You can convert to
   * the regular List type in the standard library. You can place this and
   * other functions that accept a Stream inside the Stream trait.
   *
   * @return A list of the streams elements
   */
  def toList: List[A] = foldRight(Nil:List[A])(_ :: _)

  /**
   * Exercise 5.6:
   *
   * Implement map, filter, append, and flatMap using foldRight. Because the
   * implementations are incremental, chains of transformations will avoid fully
   * instantiating the intermediate data structures.
   *
   * Let's look at a simplified program trace for (a fragment of) the motivating
   * example we started this chapter with,
   *
   * {{{
   *  Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0)
   *  (11 #:: Stream(2,3,4).map(_ + 10)).filter(_ % 2 == 0)     // apply map to first element
   *  Stream(2,3,4).map(_ + 10).filter(_ % 2 == 0)              // apply filter to first element
   *  (12 #:: Stream(3,4).map(_ + 10)).filter(_ % 2 == 0)       // apply map to second element
   *  12 #:: Stream(3,4).map(_ + 10).filter(_ % 2 == 0)         // apply filter to second element
   *  12 #:: (13 #:: Stream(4).map(_ + 10)).filter(_ % 2 == 0)
   *  12 #:: Stream(4).map(_ + 10).filter(_ % 2 == 0)
   *  12 #:: (14 #:: Stream().map(_ + 10)).filter(_ % 2 == 0)
   *  12 #:: 14 #:: Stream().map(_ + 10).filter(_ % 2 == 0)
   *  12 #:: 14 #:: Stream()
   * }}}
   *
   * Notice how the filter and map transformations are interleaved—the
   * computation alternates between generating a single element of the output of
   * map, and filter testing to see if that element is divisible by 2 (adding it
   * to the output stream if it is), exactly as if we had interleaved these bits
   * of logic in a special-purpose loop that combined both transformations.
   *
   * Notice we do not fully instantiate the intermediate stream that results
   * from the map. For this reason, people sometimes describe streams as
   * "first-class loops" whose logic can be combined using higher-order functions
   * like map and filter. The incremental nature of stream transformations also
   * has important consequences for memory usage. In a sequence of stream
   * transformations like this, the garbage collector can usually reclaim the
   * space needed for each intermediate stream element, as soon as that element
   * is passed on to the next transformation. Here, for instance, the garbage
   * collector can reclaim the space allocated for the value 13 emitted by map
   * as soon as filter determines it isn't needed. Of course, this is a simple
   * example; in other situations we might be dealing with larger numbers of
   * elements, and the stream elements themselves could be large objects that
   * retain significant amounts of memory. Being able to reclaim this memory as
   * quickly as possible can cut down on the amount of memory required by your
   * program as a whole.
   */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => {
    println(s"map on value $h") // for testing purposes at the REPL
    cons(f(h), t)
  })

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => {
    println(s"filter on value $h") // for testing purposes at the REPL
    if (p(h)) cons(h, t) else t
  })

  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((h, t) => {
    println(s"append $s") // for testing purposes at the REPL
    cons(h, t)
  })

  def flatMap[B](f:A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) =>
    f(h).uncons match {
      case Some((h2, t2)) => cons(h2, t)
      case _ => t
    }
  )

  // The given answer.
  def flatMap_1[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

  override def toString: String = uncons match {
    case Some((h, t)) => s"Stream($h, ?)"
    case _ => "Stream(Nil)"
  }

}

object Stream {

  def empty[A]: Stream[A] = new Stream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = sys.error("todo")

}