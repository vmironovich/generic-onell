package ru.ifmo.onell.problem
import scala.Ordering.Double.IeeeOrdering

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.ifmo.onell.algorithm.OnePlusOneEA

import ru.ifmo.onell.Optimizer
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, LinearRandomIntegerWeights, OneMax, OneMaxPerm, RandomPlanted3SAT}

class WModelTests extends AnyFlatSpec with Matchers {
    "WModel" should "be evaluable via its java class" in {
        val  wr : WModelRunner = new WModelRunner();
        val wrResult = wr.runWModel(100, 0.75, 4,3, 100, Array.fill[Int](100)(1))
        wr.runWModel(100, 0.75, 4,3, 100, Array.fill[Int](100)(1)) shouldBe 22.0
    }

    "WModel" should "be evaluable via its scala wrapper" in {
        val wm = new WModel(100, 0.75, 4, 3, 100)
        wm.evaluate(Array.fill[Boolean](100)(true)) shouldBe 22
    }

    "WModel" should "have optimum eq to dim in case of only epistasis set" in {
        val wm = new WModel(100, 0.0, 4, 0, 0)
        wm.isOptimalFitness(100) shouldBe (true)
    }

}
