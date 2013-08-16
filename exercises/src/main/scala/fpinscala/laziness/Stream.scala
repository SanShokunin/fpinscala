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