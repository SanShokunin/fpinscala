package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0.0)(_ + _)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _)


  /**
   * Exercise 3.2
   *
   * Implement the function tail for "removing" the first element of a List.
   * Notice the function takes constant time. What are different choices
   * you could make in your implementation if the List is Nil?
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  /**
   * Exercise 3.3
   *
   * Generalize tail to the function drop, which removes the first n elements
   * from a list.
   */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (n == 0) Cons(x, xs) else drop(xs, n - 1)
  }

  /**
   * Exercise 3.4
   *
   * Implement dropWhile,10 which removes elements from the List prefix as long
   * as they match a predicate. Again, notice these functions take time
   * proportional only to the number of elements being dropped—we do not need
   * to make a copy of the entire List.
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  /**
   * Exercise 3.5
   *
   * Using the same idea, implement the function setHead for replacing the first
   * element of a List with a different value.
   */
  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)
  }

  /**
   * Exercise 3.6
   *
   * Not everything works out so nicely. Implement a function, init, which
   * returns a List consisting of all but the last element of a List. So, given
   * List(1,2,3,4), init will return List(1,2,3). Why can't this function be
   * implemented in constant time like tail?
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /**
   * Exercise 3.9
   *
   * Compute the length of a list using foldRight
   */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  /**
   * Exercise 3.10
   *
   * The function foldRight is not tail-recursive and will StackOverflow for
   * large lists. Convince yourself that this is the case, then write another
   * general list-recursion function, foldLeft that is tail-recursive, using
   * the techniques we discussed in the previous chapter.
   */
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * Exercise 3.11
   *
   * Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  /**
   * Exercise 3.12
   *
   * Write a function that returns the reverse of a list (so given List(1,2,3)
   * it returns List(3,2,1)). See if you can write it using a fold.
   */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, elm) => Cons(elm, acc))

  /**
   * Exercise 3.13 (hard)
   *
   * Can you write foldLeft in terms of foldRight? How about the other way around?
   */
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

  /**
   * Exercise 3.14
   *
   * Implement append in terms of either foldLeft or foldRight.
   */
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  /**
   * Exercise 3.15 (hard)
   *
   * Write a function that concatenates a list of lists into a single list. Its
   * runtime should be linear in the total length of all lists. Try to use
   * functions we have already defined.
   */
  def concat[A](l: List[List[A]]): List[A] = ???

  /**
   * Exercise 3.16
   *
   * Write a function that transforms a list of integers by adding 1 to each
   * element. (Reminder: this should be a pure function that returns a new List!)
   */
  def add1(l: List[Int]): List[Int] = foldRight(l, List[Int]())((x, xs) => Cons(x + 1, xs))

  /**
   * Exercise 3.17
   *
   * Write a function that turns each value in a List[Double] into a String.
   */
  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((x, xs) => Cons(x toString, xs))

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}