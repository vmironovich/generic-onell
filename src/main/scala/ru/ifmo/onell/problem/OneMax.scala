package ru.ifmo.onell.problem

import ru.ifmo.onell.util.IntSet
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}

class OneMax(val problemSize: Int)
  extends HasEvaluation[Array[Boolean], Int]
  with HasIncrementalEvaluation[Array[Boolean], IntSet, Int]
{
  override def evaluate(individual: Array[Boolean]): Int = {
    var i, rv = 0
    val size = individual.length
    while (i < size) {
      if (individual(i)) rv += 1
      i += 1
    }
    rv
  }

  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs
  override def isOptimalFitness(fitness: Int): Boolean = fitness == problemSize
  override def numberOfChangesForProblemSize(problemSize: Int): Int = problemSize

  override def applyDelta(ind: Array[Boolean], delta: IntSet, currentFitness: Int): Int = {
    val size = delta.size
    var newFitness = currentFitness
    var i = 0
    while (i < size) {
      val idx = delta(i).toInt
      newFitness += (if (ind(idx)) -1 else 1)
      ind(idx) ^= true
      i += 1
    }
    newFitness
  }

  override def unapplyDelta(ind: Array[Boolean], delta: IntSet): Unit = {
    val size = delta.size
    var i = 0
    while (i < size) {
      ind(delta(i).toInt) ^= true
      i += 1
    }
  }

  override def evaluateAssumingDelta(ind: Array[Boolean], delta: IntSet, currentFitness: Int): Int = {
    val size = delta.size
    var newFitness = currentFitness
    var i = 0
    while (i < size) {
      val idx = delta(i).toInt
      newFitness += (if (ind(idx)) -1 else 1)
      i += 1
    }
    newFitness
  }
}
