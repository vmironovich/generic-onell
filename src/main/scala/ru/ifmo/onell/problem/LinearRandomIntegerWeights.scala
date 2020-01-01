package ru.ifmo.onell.problem

import java.util.Random

import ru.ifmo.onell.util.{Helpers, OrderedSet}
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}

import LinearRandomIntegerWeights.FAHD

class LinearRandomIntegerWeights(val problemSize: Int, val maxWeight: Int, randomSeed: Long)
  extends HasEvaluation[Array[Boolean], FAHD]
    with HasIncrementalEvaluation[Array[Boolean], Int, FAHD]
{
  private[this] val rng = new Random(randomSeed)
  private[this] val weights = Array.fill(problemSize)(rng.nextInt(maxWeight) + 1)
  private[this] val weightSum = weights.sum

  override def evaluate(individual: Array[Boolean]): FAHD = {
    var i = individual.length - 1
    var fitness = 0L
    var distance = 0
    while (i >= 0) {
      if (individual(i)) {
        fitness += weights(i)
      } else {
        distance += 1
      }
      i -= 1
    }
    new FAHD(fitness, distance)
  }

  override def compare(lhs: FAHD, rhs: FAHD): Int = java.lang.Long.compare(lhs.fitness, rhs.fitness)
  override def isOptimalFitness(fitness: FAHD): Boolean = fitness.fitness == weightSum
  override def numberOfChangesForProblemSize(problemSize: Int): Int = problemSize
  override def sizeTypeToLong(st: Int): Long = st

  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: FAHD): FAHD = {
    val size = delta.size
    var newFitness = currentFitness.fitness
    var newDistance = currentFitness.distance
    var i = 0
    while (i < size) {
      val idx = delta(i)
      if (ind(idx)) {
        newFitness -= weights(idx)
        newDistance += 1
      } else {
        newFitness += weights(idx)
        newDistance -= 1
      }
      ind(idx) ^= true
      i += 1
    }
    new FAHD(newFitness, newDistance)
  }

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = Helpers.flipEachBit(ind, delta)

  override def evaluateAssumingDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: FAHD): FAHD = {
    val size = delta.size
    var newFitness = currentFitness.fitness
    var newDistance = currentFitness.distance
    var i = 0
    while (i < size) {
      val idx = delta(i)
      if (ind(idx)) {
        newFitness -= weights(idx)
        newDistance += 1
      } else {
        newFitness += weights(idx)
        newDistance -= 1
      }
      i += 1
    }
    new FAHD(newFitness, newDistance)
  }
}

object LinearRandomIntegerWeights {
  final class FAHD(val fitness: Long, val distance: Int)
}
