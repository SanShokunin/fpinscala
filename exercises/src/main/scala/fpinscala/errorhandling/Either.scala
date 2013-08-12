package fpinscala.errorhandling

sealed trait Either[+E,+A] {

  /**
   * Exercise 4.7
   *
   * Implement versions of map, flatMap, orElse, and map2 on Either that
   * operate on the Right value.
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a,b1)

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this flatMap( a => b map ( b => f(a,b)))

}

case class Left[+E](get: E) extends Either[E,Nothing]

case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!") 
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] = 
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }

  /**
   * Exercise 4.8
   *
   * Implement sequence for Either (version with pattern matching and explicit
   * recursion).
   */
  def sequence[E,A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case x :: xs => x.map2(sequence(xs))(_ :: _)
  }

  /**
   * Exercise 4.8
   *
   * Implement sequence for Either (version with foldRight).
   */
  def sequence_1[E,A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldRight[Either[E,List[A]]](Right(Nil))((a, b) => a.map2(b)(_ :: _))

}