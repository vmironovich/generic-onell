package ru.ifmo.onell.algorithm

import scala.Ordering.Double.IeeeOrdering

import org.scalatest.{FlatSpec, Matchers}

import ru.ifmo.onell.problem.{OneMax, OneMaxPerm}

class OnePlusLambdaLambdaGATests extends FlatSpec with Matchers {
  "(1+LL) GA" should "perform as expected on OneMax" in {
    val size = 200
    val om = new OneMax(size)
    val ll = new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda)
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1300.0)
  }

  it should "perform as expected on OneMaxPerm" in {
    val size = 200
    val om = new OneMaxPerm(size)
    val ll = new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda)
    val runs = IndexedSeq.fill(10)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1.1e5)
  }
}
