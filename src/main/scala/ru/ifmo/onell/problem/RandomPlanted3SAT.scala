package ru.ifmo.onell.problem

import java.util.concurrent.ThreadLocalRandom

import ru.ifmo.onell.util.IntSet
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}

import scala.annotation.tailrec

class RandomPlanted3SAT(val problemSize: Int, val clauseCount: Int)
  extends HasEvaluation[Array[Boolean], Int] with HasIncrementalEvaluation[Array[Boolean], IntSet, Int]
{
  private[this] val assignment = Array.fill(problemSize)(ThreadLocalRandom.current().nextBoolean())
  private[this] val clauseVar = new Array[Int](clauseCount * 3)
  private[this] val clauseVal = new Array[Boolean](clauseCount * 3)
  private[this] val clausesOfVariableOffset = new Array[Int](problemSize + 1)
  private[this] val clausesOfVariableContent = new Array[Int](clauseCount * 3)

  // initialization
  generateClauses(0, ThreadLocalRandom.current())
  makePartialSums(0)
  populateClausesOfVariableContent(0)

  private[this] def isClauseSatisfied(clauseOffset: Int, solution: Array[Boolean]): Boolean = {
    val i0 = clauseOffset
    val i1 = clauseOffset + 1
    val i2 = clauseOffset + 2
    solution(clauseVar(i0)) == clauseVal(i0) ||
      solution(clauseVar(i1)) == clauseVal(i1) ||
      solution(clauseVar(i2)) == clauseVal(i2)
  }

  @tailrec
  private[this] def generateClauses(offset: Int, rng: ThreadLocalRandom): Unit = if (offset < clauseVal.length) {
    clauseVar(offset) = rng.nextInt(problemSize)
    clauseVar(offset + 1) = rng.nextInt(problemSize)
    clauseVar(offset + 2) = rng.nextInt(problemSize)
    clauseVal(offset) = rng.nextBoolean()
    clauseVal(offset + 1) = rng.nextBoolean()
    clauseVal(offset + 2) = rng.nextBoolean()
    if (isClauseSatisfied(offset, assignment)) {
      clausesOfVariableOffset(clauseVar(offset)) += 1
      clausesOfVariableOffset(clauseVar(offset + 1)) += 1
      clausesOfVariableOffset(clauseVar(offset + 2)) += 1
      generateClauses(offset + 3, rng)
    } else {
      generateClauses(offset, rng)
    }
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
    clausesOfVariableContent(offset) = (i / 3) * 3
    populateClausesOfVariableContent(i + 1)
  }

  override def compare(lhs: Int, rhs: Int): Int = Integer.compare(lhs, rhs)
  override def isOptimalFitness(fitness: Int): Boolean = fitness == clauseCount
  override def numberOfChangesForProblemSize(problemSize: Int): Long = problemSize

  override def evaluate(individual: Array[Boolean]): Int = {
    (0 until clauseCount).count(i => isClauseSatisfied(3 * i, individual))
  }

  /**
    * Applies the given delta to the given individual, while simultaneously recomputing the fitness.
    *
    * @param ind            the (mutable) individual.
    * @param delta          the delta (the description of a change to the individual).
    * @param currentFitness the fitness of the individual before applying the change.
    * @return the new fitness after applying the change; the individual is also changed when the method returns.
    */
  override def applyDelta(ind: Array[Boolean], delta: IntSet, currentFitness: Int): Int = ???

  /**
    * Unapplies the given delta to the given individual.
    *
    * @param ind   the (mutable) individual, which has previously experienced applying exactly the same delta.
    * @param delta the delta to be unapplied.
    */
  override def unapplyDelta(ind: Array[Boolean], delta: IntSet): Unit = ???
}
