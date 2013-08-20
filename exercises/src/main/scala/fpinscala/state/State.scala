package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
                  ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
       simple(seed2))
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
   * Exercise 6.1:
   *
   * Write a function to generate a random positive integer. Note: you can use
   * x.abs to take the absolute value of an Int, x. Make sure to handle the
   * corner case Int.MinValue, which doesn't have a positive counterpart.
   *
   * @param rng Random Number Generator [RNG]
   * @return Tuple of generated positive Integer and a new RNG
   */
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (next, newRng) = rng.nextInt
    if (next == Int.MinValue) positiveInt(newRng) else (next.abs, newRng)
  }

  /**
   * Exercise 6.2:
   *
   * Write a function to generate a Double between 0 and 1, not including 1.
   *
   * Note: you can use Int.MaxValue to obtain the maximum positive integer value
   * and you can use x.toDouble to convert an Int, x, to a Double.
   *
   * @param rng Random Number Generator [RNG]
   * @return Tuple of generated Double and a new RNG
   */
  def double(rng: RNG): (Double, RNG) = {
    val (next, newRng) = rng.nextInt
    (next / (Int.MaxValue.toDouble + 1), newRng)
  }

  /**
   * Exercise 6.3:
   *
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
   * and a (Double, Double, Double) 3-tuple. You should be able to reuse the
   * functions you've already written.
   */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, newRng) = rng.nextInt
    val (d, newRng2) = double(newRng)
    ((i, d), newRng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), newRng) = intDouble(rng)
    ((d, i), newRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, newRng) = double(rng)
    val (d2, newRng2) = double(newRng)
    val (d3, newRng3) = double(newRng2)
    ((d1, d2, d3), newRng3)
  }

  /**
   * Exercise 6.4:
   *
   * Write a function to generate a list of random integers.
   *
   * @param count
   * @param rng
   * @return
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, ints: List[Int]): (List[Int], RNG) =
      if (count == 0) (ints, rng)
      else {
        val (i, r) = rng.nextInt
        go(count - 1, r, i :: ints)
      }
    go(count, rng, Nil)
  }

  def positiveMax(n: Int): Rand[Int] = sys.error("todo")

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = sys.error("todo")

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = sys.error("todo")

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = sys.error("todo")

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")
}