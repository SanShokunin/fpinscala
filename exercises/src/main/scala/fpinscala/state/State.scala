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

  /**
   * Looking at our implementations of exercises 6.{1,2,3,4}, we notice a common
   * pattern:
   *
   * each of our functions has a type of the form RNG => (A, RNG) for some type A.
   *
   * Functions of this type describe state actions that transform RNG states, and
   * these state actions can be built up and combined using general-purpose
   * functions. To make them convenient to talk about, let's make a type alias for
   * the RNG state action data type: `Rand[+A]`.
   *
   * We can now turn methods such as RNG's nextInt into values of this type:
   *
   * {{{val int: Rand[Int] = _.nextInt}}}
   *
   * @tparam A
   */
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  /**
   * A simple RNG-transition is the unit action, which passes the RNG state
   * through without using it, always returning a constant value rather than a
   * random value.
   *
   * In `sequence`, the base case of the fold is a `unit` action that returns
   * the empty list.
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /**
   * There is also map, for transforming the output of a state action without
   * modifying the state itself. Remember, Rand[A] is just a type alias for a
   * function type RNG => (A, RNG), so this is just a kind of function
   * composition.
   */
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

  /**
   * Exercise 6.5:
   *
   * Use map to generate an Int between 0 and n, inclusive.
   *
   * @param n The upper bound integer value.
   * @return type Rand[+A], i.e. RNG => (A, RNG)
   */
  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ / (Int.MaxValue / n))

  /**
   * Exercise 6.6:
   *
   * Use map to reimplement RNG.double in a more elegant way.
   */
  val doubleViaMap: Rand[Double] = map(int)(_ / (Int.MaxValue.toDouble + 1))

  /**
   * Exercise 6.7:
   *
   * Unfortunately, map is not powerful enough to implement intDouble and
   * doubleInt from before. What we need is a new combinator map2, that can
   * combine two RNG actions into one using a binary rather than unary function.
   * Write its implementation and then use it to reimplement the intDouble and
   * doubleInt functions.
   *
   * @param ra
   * @param rb
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  val intDoubleViaMap2: Rand[(Int, Double)] = map2(int, double)((i, d) => (i, d))

  val doubleIntViaMap2: Rand[(Double, Int)] = map2(int, double)((i, d) => (d, i))

  /**
   * Exercise 6.8 (hard):
   *
   * If we can combine two RNG transitions, we should be able to combine a whole
   * list of them. Implement `sequence, for combining a List of transitions into
   * a single transition.
   *
   * Use it to reimplement the ints function you wrote before. For the latter,
   * you can use the standard library function `List.fill(n)(x)` to make a list
   * with x repeated n times.
   *
   * In `sequence`, the base case of the fold is a `unit` action that returns
   * the empty list. At each step in the fold, we accumulate in `acc` and `f`
   * is the current element in the list. `map2(f, acc)(_ :: _)` results in a
   * value of type `Rand[List[A]]`. We map over that to prepend (cons) the
   * element onto the accumulated list. We are using `foldRight`. If we used
   * `foldLeft` then the values in the resulting list would appear in reverse
   * order.
   *
   * @param fs
   * @tparam A
   * @return
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil:List[A]))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  /**
   * Exercise 6.9:
   *
   * Implement flatMap, then use it to reimplement positiveInt.
   *
   * We're starting to see a pattern: We're progressing towards implementations
   * that don't explicitly mention or pass along the RNG value. The map and
   * map2 combinators allowed us to implement, in a rather succinct and elegant
   * way, functions that were otherwise tedious and error-prone to write. But
   * there are some functions that we can't very well write in terms of map and
   * map2.
   *
   * Let's go back to positiveInt and see if it can be implemented in terms of
   * map. It's possible to get most of the way there, but what do we do in the
   * case that Int.MinValue is generated? It doesn't have a positive counterpart
   * and we can't just select an arbitrary number:
   *
   * {{{
   *   def positiveInt: Rand[Int] = {
   *     map(int) { i =>
   *       if (i != Int.MinValue) i.abs else ?? // What goes here?
   *     }
   *   }
   * }}}
   *
   * We want to retry the generator in the case of Int.MinValue, but we don't
   * actually have an RNG! Besides, anything except an Int there would have the
   * wrong type. So we clearly need a more powerful combinator than map.
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r2) = f(rng)
      // Note: The appending `(r2)` executes the function `rng => (a, rng)`
      // returned by `g(a)` which finally results in a `(B, RNG)` expression.
      g(a)(r2)
    }

  def positiveIntViaFlatMap: Rand[Int] = flatMap(int) { a =>
    a match {
      case Int.MinValue => positiveIntViaFlatMap
      case i => unit(i.abs)
    }
  }

  /**
   * Exercise 6.10:
   *
   * Reimplement map and map2 in terms of flatMap.
   */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
 * Exercise 6.11:
 *
 * Generalize the functions unit, map, map2, flatMap, and sequence. Add them as
 * methods on the State case class where possible. Otherwise you should put
 * them in a State companion object.
 *
 * The functions you've just written, unit, map, map2, flatMap, and sequence,
 * are not really specific to random number generation at all. They are general-
 * purpose functions for working with state actions, and don't actually care
 * about the type of the state.
 */
case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil))((s, acc) => s.map2(acc)(_ :: _))

  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")

}