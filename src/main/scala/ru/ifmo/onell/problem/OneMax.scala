package ru.ifmo.onell.problem

import ru.ifmo.onell.util.IntArraySet
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}

class OneMax(size: Int)
  extends HasEvaluation[Array[Boolean], Int]
  with HasIncrementalEvaluation[Array[Boolean], IntArraySet, Int]
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
  override def problemSize: Int = size
  override def isOptimalFitness(fitness: Int): Boolean = fitness == size

  override def applyDelta(ind: Array[Boolean], delta: IntArraySet, currentFitness: Int): Int = {
    val size = delta.size
    var newFitness = currentFitness
    var i = 0
    while (i < size) {
      val idx = delta(i)
      newFitness += (if (ind(idx)) -1 else 1)
      ind(idx) ^= true
      i += 1
    }
    newFitness
  }

  override def unapplyDelta(ind: Array[Boolean], delta: IntArraySet): Unit = for (i <- delta) ind(i) ^= true
}
