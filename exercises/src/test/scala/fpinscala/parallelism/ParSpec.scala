package fpinscala.parallelism

import org.scalatest.FunSuite
import java.util.concurrent.{ExecutorService, Executors}
import fpinscala.parallelism.Par.{Par, UnitFuture}

class ParSpec extends FunSuite {

  import Examples._
  val es = Executors.newFixedThreadPool(4)
  val seq = IndexedSeq(1,2,3,4)

  ignore("parallel summation") {
    val sum = parallelSum(seq)
    assert(sum(es).get() === 10)
  }

  test("convert a function A => B to one that evaluates its result asynchronously") {
    val asyncF = Par.asyncF((i: Int) => i * 2)
    assert(asyncF(3)(es).get === 6)
  }

  test("sort Par[List[Int]]") {
    val par: Par[List[Int]] = (es: ExecutorService) => UnitFuture(List(2,3,4,1))
    assert(Par.sortPar(par)(es).get === List(1,2,3,4))
  }

}
