package ru.ifmo.onell.problem

import java.util.Random

import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}
import ru.ifmo.onell.util.{Helpers, OrderedSet}

class LinearRandomDoubleWeights(val problemSize: Int, val maxWeight: Double, randomSeed: Long)
  extends HasEvaluation[Array[Boolean], Double]
    with HasIncrementalEvaluation[Array[Boolean], Int, Double]
{
  private[this] val rng = new Random(randomSeed)
  private[this] val weights = Array.fill(problemSize)(rng.nextDouble() * (maxWeight - 1) + 1)
  private[this] val weightSum = weights.sum

  override def evaluate(individual: Array[Boolean]): Double = {
    var i = individual.length - 1
    var rv = 0.0
    while (i >= 0) {
      if (individual(i)) rv += weights(i)
      i -= 1
    }
    rv
  }

  override def compare(lhs: Double, rhs: Double): Int = java.lang.Double.compare(lhs, rhs)
  override def isOptimalFitness(fitness: Double): Boolean = fitness > weightSum - 1e-1
  override def numberOfChangesForProblemSize(problemSize: Int): Int = problemSize
  override def sizeTypeToLong(st: Int): Long = st

  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Double): Double = {
    val size = delta.size
    var newFitness = currentFitness
    var i = 0
    while (i < size) {
      val idx = delta(i)
      newFitness += (if (ind(idx)) -1 else 1) * weights(idx)
      ind(idx) ^= true
      i += 1
    }
    newFitness
  }

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = Helpers.flipEachBit(ind, delta)

  override def evaluateAssumingDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Double): Double = {
    val size = delta.size
    var newFitness = currentFitness
    var i = 0
    while (i < size) {
      val idx = delta(i)
      newFitness += (if (ind(idx)) -1 else 1) * weights(idx)
      i += 1
    }
    newFitness
  }
}
