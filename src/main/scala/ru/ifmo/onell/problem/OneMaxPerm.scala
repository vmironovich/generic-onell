package ru.ifmo.onell.problem

import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}
import ru.ifmo.onell.util.{IntSet, Permutation}

class OneMaxPerm(val problemSize: Int)
  extends HasEvaluation[Permutation, Int]
    with HasIncrementalEvaluation[Permutation, IntSet, Int]
{
  override def evaluate(individual: Permutation): Int = {
    var i, rv = 0
    while (i < individual.size) {
      if (individual(i) == i) {
        rv += 1
      }
      i += 1
    }
    rv
  }

  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs
  override def isOptimalFitness(fitness: Int): Boolean = fitness == problemSize
  override def numberOfChangesForProblemSize(problemSize: Int): Int = problemSize * (problemSize - 1) / 2

  override def applyDelta(ind: Permutation, delta: IntSet, currentFitness: Int): Int = {
    var i = 0
    var newFitness = currentFitness
    while (i < delta.size) {
      val (j0, j1) = OneMaxPerm.unpack(delta(i))
      if (ind(j0) == j0) newFitness -= 1
      if (ind(j1) == j1) newFitness -= 1
      ind.swap(j0, j1)
      if (ind(j0) == j0) newFitness += 1
      if (ind(j1) == j1) newFitness += 1
      i += 1
    }
    newFitness
  }

  override def unapplyDelta(ind: Permutation, delta: IntSet): Unit = {
    var i = delta.size - 1
    while (i >= 0) {
      val (j0, j1) = OneMaxPerm.unpack(delta(i))
      ind.swap(j0, j1)
      i -= 1
    }
  }
}

object OneMaxPerm {
  private[problem] def unpack(change: Long): (Int, Int) = {
    val bigger = ((1 + math.sqrt(1 + 8.0 * change)) / 2).toInt
    val smaller = change - bigger * (bigger - 1) / 2
    assert(bigger > smaller)
    (smaller.toInt, bigger)
  }
}
