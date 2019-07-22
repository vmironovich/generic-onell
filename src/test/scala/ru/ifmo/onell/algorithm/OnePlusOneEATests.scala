package ru.ifmo.onell.algorithm

import scala.Ordering.Double.IeeeOrdering

import org.scalatest.{FlatSpec, Matchers}
import ru.ifmo.onell.problem.OneMax

class OnePlusOneEATests extends FlatSpec with Matchers {
  "(1+1) EA" should "perform as expected on OneMax" in {
    val size = 200
    val om = new OneMax(size)
    val runs = IndexedSeq.fill(100)(OnePlusOneEA.optimize(om))
    val expected = size * (1 to size / 2).map(1.0 / _).sum * (math.E - 1)
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected)
  }
}
