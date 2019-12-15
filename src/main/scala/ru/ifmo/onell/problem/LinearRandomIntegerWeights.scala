package ru.ifmo.onell.problem

import java.util.Random

import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}

class LinearRandomIntegerWeights(val problemSize: Int, val maxWeight: Int, randomSeed: Long)
  extends HasEvaluation[Array[Boolean], Long]
    with HasIncrementalEvaluation[Array[Boolean], Int, Int, Long]
{
  private[this] val rng = new Random(randomSeed)
  private[this] val weights = Array.fill(problemSize)(rng.nextInt(maxWeight - 1) + 1)
  private[this] val weightSum = weights.sum

  override def evaluate(individual: Array[Boolean]): Long = {
    var i = individual.length - 1
    var rv = 0L
    while (i >= 0) {
      if (individual(i)) rv += weights(i)
      i -= 1
    }
    rv
  }

  override def compare(lhs: Long, rhs: Long): Int = lhs.compare(rhs)
  override def isOptimalFitness(fitness: Long): Boolean = fitness == weightSum
  override def numberOfChangesForProblemSize(problemSize: Int): Int = problemSize
  override def sizeTypeToLong(st: Int): Long = st

  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Long): Long = {
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

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = {
    val size = delta.size
    var i = 0
    while (i < size) {
      ind(delta(i)) ^= true
      i += 1
    }
  }

  override def evaluateAssumingDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Long): Long = {
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
