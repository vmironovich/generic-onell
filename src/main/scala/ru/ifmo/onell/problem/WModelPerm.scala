package ru.ifmo.onell.problem

import ru.ifmo.onell.Fitness
import ru.ifmo.onell.util.{OrderedSet, Permutation}

class WModelPerm(val problemSize: Int, dummy_para : Double, epistasis_para : Int,
neutrality_para : Int, ruggedness_para : Int)
  extends Fitness[Permutation, Int, Long]
{
  override def worstFitness: Int = -1
  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs
  override def numberOfChanges: Long = problemSize * (problemSize - 1L) / 2
  override def changeIndexTypeToLong(st: Long): Long = st

  val wmRunner = new WModelRunner()
  val optimalValue : Int = wmRunner.getOptimum(problemSize, dummy_para,  epistasis_para, neutrality_para, ruggedness_para).toInt
  override def isOptimalFitness(fitness: Int): Boolean = fitness == optimalValue


  override def evaluate(individual: Permutation): Int = {
    val omInd = new Array[Int](problemSize)
    var i = 0
    val size = individual.size
    while (i < size) {
      if (individual(i) == i) omInd(i)=1
      i += 1
    }
    wmRunner.runWModel(problemSize, dummy_para,  epistasis_para, neutrality_para, ruggedness_para, omInd).toInt
  }

  override def applyDelta(ind: Permutation, delta: OrderedSet[Long], currentFitness: Int): Int = {
    var i = 0
    while (i < delta.size) {
      val di = delta(i)
      val j1 = OneMaxPerm.getBigger(di)
      val j0 = OneMaxPerm.getSmaller(di, j1)
      ind.swap(j0, j1)
      i += 1
    }
    evaluate(ind)
  }

  override def unapplyDelta(ind: Permutation, delta: OrderedSet[Long]): Unit = {
    var i = delta.size - 1
    while (i >= 0) {
      val di = delta(i)
      val j1 = OneMaxPerm.getBigger(di)
      val j0 = OneMaxPerm.getSmaller(di, j1)
      ind.swap(j0, j1)
      i -= 1
    }
  }
}
