package ru.ifmo.onell.problem

import ru.ifmo.onell.util.{Helpers, OrderedSet}
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}

class OneMax(val problemSize: Int)
  extends HasEvaluation[Array[Boolean], Int]
  with HasIncrementalEvaluation[Array[Boolean], Int, Int]
{
  override def evaluate(individual: Array[Boolean]): Int = Helpers.countTrueBits(individual)
  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs
  override def isOptimalFitness(fitness: Int): Boolean = fitness == problemSize
  override def numberOfChangesForProblemSize(problemSize: Int): Int = problemSize
  override def sizeTypeToLong(st: Int): Long = st

  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Int): Int = {
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

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = Helpers.flipEachBit(ind, delta)

  override def evaluateAssumingDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Int): Int =
    currentFitness + Helpers.countChanges(ind, delta)
}
