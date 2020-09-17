package ru.ifmo.onell.problem

import ru.ifmo.onell.Fitness
import ru.ifmo.onell.util.{Helpers, OrderedSet}
import scala.collection.mutable.ArrayBuffer

class WModel(val problemSize: Int, dummy_para : Double, epistasis_para : Int,
neutrality_para : Int, ruggedness_para : Int) extends Fitness[Array[Boolean], Int, Int]
{
  override def worstFitness: Int = -1
  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs
  override def numberOfChanges: Int = problemSize
  override def changeIndexTypeToLong(st: Int): Long = st

  val wmRunner = new WModelRunner()

  override def evaluate(individual: Array[Boolean]): Int = wmRunner.runWModel(problemSize, dummy_para,  epistasis_para, neutrality_para, ruggedness_para, individual.map(i => if (i) 1 else 0)).toInt
  val optimalValue : Int = wmRunner.getOptimum(problemSize, dummy_para,  epistasis_para, neutrality_para, ruggedness_para).toInt
  override def isOptimalFitness(fitness: Int): Boolean = fitness == optimalValue



  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Int): Int = {
    Helpers.flipEachBit(ind, delta)
    evaluate(ind)
  }

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = Helpers.flipEachBit(ind, delta)
}
