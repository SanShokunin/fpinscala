package fpinscala.parallelism

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /* Simple future for wrapping a constant value. */
  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {}

  /**
   * Exercise 7.3:
   *
   * Let's begin by implementing the functions of the API we've developed so far.
   * Now that we have a representation for Par, we should be able to fill these
   * in.
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def async[A](a: => A): Par[A] = fork(unit(a))

  /**
   * Exercise 7.4:
   *
   * This API already enables a rich set of operations. Here's a simple
   * example: using async, write a function to convert any function A => B to
   * one that evaluates its result asynchronously
   */
  def asyncF[A,B](f: A => B): A => Par[B] = (a) => async(f(a))

  def map[A,B](fa: Par[A])(f: A => B): Par[B] = ???

  def sortPar(l: Par[List[Int]]) = ???

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = ???

  def delay[A](fa: => Par[A]): Par[A] = ???

}

object Examples {

  import Par._

  def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0 // Hints and standalone answers
    else { 
      val (l,r) = as.splitAt(as.length/2) 
      sum(l) + sum(r)
    }

  def parallelSum(as: IndexedSeq[Int]): Par[Int] =
    if (as.isEmpty) Par.unit(0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(fork(parallelSum(l)), fork(parallelSum(r)))(_ + _)
    }
}