package ru.ifmo.onell.algorithm

import scala.Ordering.Double.IeeeOrdering

import ru.ifmo.onell.problem.OneMax
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RLSTests extends AnyFlatSpec with Matchers {
  "RLS" should "perform as expected on OneMax" in {
    val size = 200
    val om = new OneMax(size)
    val runs = IndexedSeq.fill(100)(RLS.optimize(om))
    val expected = size * (1 to size / 2).map(1.0 / _).sum
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected * 1.1)
  }
}
