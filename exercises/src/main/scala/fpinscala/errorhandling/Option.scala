package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  
  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }
  
  def orElse[B>:A](ob: => Option[B]): Option[B] = this map(Some(_)) getOrElse ob
  
  /*
  Again, we can implement this with explicit pattern matching. 
  */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
  
  def filter(f: A => Boolean): Option[A] = map(a => if (f(a)) Some(a) else None) getOrElse None

  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val x: Int = throw new Exception("fail!")
    try {
      val y = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] = 
    pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.

  def mkMatcher_1(pat: String): Option[String => Boolean] = 
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)
  
  def doesMatch(pat: String, s: String): Option[Boolean] = 
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f => 
    mkMatcher(pat2) map     (g => 
    f(s) && g(s)))
  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  /**
   * Exercise 4.3
   *
   * Write a generic function map2, that combines two Option values using a
   * binary function. If either Option value is None, then the return value is
   * too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap(a => b map(b => f(a,b)))

  /**
   * Exercise 4.4
   *
   * Re-implement bothMatch above in terms of this new function, to the extent possible.
   */
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = map2(mkMatcher(pat1), mkMatcher(pat2))(_(s) && _(s))

  /**
   * Exercise 4.5
   *
   * Write a function sequence, that combines a list of Options into one option
   * containing a list of all the Some values in the original list. If the
   * original list contains None even once, the result of the function should
   * be None, otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Some(x) :: xs if (!xs.contains(None)) => Some(x :: sequence(xs).getOrElse(Nil))
    case _ => None
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }
}