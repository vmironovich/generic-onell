package ru.ifmo.onell.problem

import java.util.Random

import scala.annotation.tailrec

import ru.ifmo.onell.Fitness
import ru.ifmo.onell.util.{DenseIntSet, Helpers, OrderedSet}

class RandomPlanted3SAT(val problemSize: Int, val clauseCount: Int,
                        valueGenerator: RandomPlanted3SAT.ValueGenerator, randomSeed: Long)
  extends Fitness[Array[Boolean], Int, Int]
{
  private[this] val clauseVar = new Array[Int](clauseCount * 3)
  private[this] val clauseVal = new Array[Boolean](clauseCount * 3)
  private[this] val clausesOfVariableOffset = new Array[Int](problemSize + 1)
  private[this] val clausesOfVariableContent = new Array[Int](clauseCount * 3)
  private[this] val usedClauses = new DenseIntSet(clauseCount)

  // initialization
  generateClauses(0, new Random(randomSeed))
  makePartialSums(0)
  populateClausesOfVariableContent(0)

  private[this] def isClauseSatisfied(clauseIndex: Int, solution: Array[Boolean]): Boolean = {
    val clauseOffset = 3 * clauseIndex
    val i0 = clauseOffset
    val i1 = clauseOffset + 1
    val i2 = clauseOffset + 2
    solution(clauseVar(i0)) == clauseVal(i0) ||
      solution(clauseVar(i1)) == clauseVal(i1) ||
      solution(clauseVar(i2)) == clauseVal(i2)
  }

  @tailrec
  private[this] def generateClauses(clauseIdx: Int, rng: Random): Unit = if (clauseIdx < clauseCount) {
    val offset = 3 * clauseIdx
    val i0, i1, i2 = rng.nextInt(problemSize)
    clauseVar(offset) = i0
    clauseVar(offset + 1) = i1
    clauseVar(offset + 2) = i2
    valueGenerator.generateThree(clauseVal, offset, rng)
    assert(clauseVal(offset) || clauseVal(offset + 1) || clauseVal(offset + 2))
    clausesOfVariableOffset(i0) += 1
    clausesOfVariableOffset(i1) += 1
    clausesOfVariableOffset(i2) += 1
    generateClauses(clauseIdx + 1, rng)
  }

  @tailrec
  private[this] def makePartialSums(i: Int): Unit = if (i < problemSize) {
    clausesOfVariableOffset(i + 1) += clausesOfVariableOffset(i)
    makePartialSums(i + 1)
  }

  @tailrec
  private[this] def populateClausesOfVariableContent(i: Int): Unit = if (i < clauseVar.length) {
    val variable = clauseVar(i)
    val offset = clausesOfVariableOffset(variable) - 1
    clausesOfVariableOffset(variable) = offset
    clausesOfVariableContent(offset) = i / 3
    populateClausesOfVariableContent(i + 1)
  }

  override def worstFitness: Int = -1
  override def compare(lhs: Int, rhs: Int): Int = Integer.compare(lhs, rhs)
  override def isOptimalFitness(fitness: Int): Boolean = fitness == clauseCount
  override def numberOfChanges: Int = problemSize
  override def changeIndexTypeToLong(st: Int): Long = st

  override def evaluate(individual: Array[Boolean]): Int = {
    (0 until clauseCount).count(i => isClauseSatisfied(i, individual))
  }

  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Int): Int = {
    usedClauses.clear()
    val size = delta.size
    var i = 0
    while (i < size) {
      val d = delta(i)
      var j = clausesOfVariableOffset(d)
      val jMax = clausesOfVariableOffset(d + 1)
      while (j < jMax) {
        usedClauses.add(clausesOfVariableContent(j))
        j += 1
      }
      i += 1
    }
    val satBefore = countSatisfiedClauses(ind)
    Helpers.flipEachBit(ind, delta)
    currentFitness - satBefore + countSatisfiedClauses(ind)
  }

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = Helpers.flipEachBit(ind, delta)

  private def countSatisfiedClauses(individual: Array[Boolean]): Int = {
    var f = 0
    var i = usedClauses.size - 1
    while (i >= 0) {
      f += (if (isClauseSatisfied(usedClauses(i), individual)) 1 else 0)
      i -= 1
    }
    f
  }
}

object RandomPlanted3SAT {
  trait ValueGenerator {
    def generateThree(targetArray: Array[Boolean], offset: Int, rng: Random): Unit
  }

  object EasyGenerator extends ValueGenerator {
    @scala.annotation.tailrec
    override def generateThree(targetArray: Array[Boolean], offset: Int, rng: Random): Unit = {
      val v0, v1, v2 = rng.nextBoolean()
      if (v0 || v1 || v2) {
        targetArray(offset) = v0
        targetArray(offset + 1) = v1
        targetArray(offset + 2) = v2
      } else {
        generateThree(targetArray, offset, rng)
      }
    }
  }

  object HardGenerator extends ValueGenerator {
    override def generateThree(targetArray: Array[Boolean], offset: Int, rng: Random): Unit = {
      rng.nextInt(6) match {
        case 0 =>
          targetArray(offset) = true
          targetArray(offset + 1) = true
          targetArray(offset + 2) = true
        case 1 =>
          targetArray(offset) = true
          targetArray(offset + 1) = true
          targetArray(offset + 2) = true
          targetArray(offset + rng.nextInt(3)) = false
        case _ =>
          targetArray(offset + rng.nextInt(3)) = true
      }
    }
  }
}
