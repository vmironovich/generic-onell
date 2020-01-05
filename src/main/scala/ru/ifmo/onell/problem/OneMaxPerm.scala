package ru.ifmo.onell.problem

import ru.ifmo.onell.util.{OrderedSet, Permutation}
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}

class OneMaxPerm(val problemSize: Int)
  extends HasEvaluation[Permutation, Int]
    with HasIncrementalEvaluation[Permutation, Long, Int]
{
  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs
  override def isOptimalFitness(fitness: Int): Boolean = fitness == problemSize
  override def numberOfChangesForProblemSize(problemSize: Int): Long = problemSize * (problemSize - 1L) / 2
  override def sizeTypeToLong(st: Long): Long = st

  override def evaluate(individual: Permutation): Int = {
    var i, rv = 0
    val size = individual.size
    while (i < size) {
      if (individual(i) == i) rv += 1
      i += 1
    }
    rv
  }

  override def applyDelta(ind: Permutation, delta: OrderedSet[Long], currentFitness: Int): Int = {
    var i = 0
    var newFitness = currentFitness
    while (i < delta.size) {
      val j0j1 = OneMaxPerm.unpack(delta(i))
      val j0 = j0j1.toInt
      val j1 = (j0j1 >>> 32).toInt

      if (ind(j0) == j0) newFitness -= 1
      if (ind(j1) == j1) newFitness -= 1
      ind.swap(j0, j1)
      if (ind(j0) == j0) newFitness += 1
      if (ind(j1) == j1) newFitness += 1
      i += 1
    }
    newFitness
  }

  override def unapplyDelta(ind: Permutation, delta: OrderedSet[Long]): Unit = {
    var i = delta.size - 1
    while (i >= 0) {
      val j0j1 = OneMaxPerm.unpack(delta(i))
      val j0 = j0j1.toInt
      val j1 = (j0j1 >>> 32).toInt
      ind.swap(j0, j1)
      i -= 1
    }
  }
}

object OneMaxPerm {
  def unpack(change: Long): Long = {
    val bigger = ((1 + math.sqrt(1 + 8.0 * change)) / 2).toLong
    val smaller = change - bigger * (bigger - 1) / 2
    smaller ^ (bigger << 32)
  }
}
