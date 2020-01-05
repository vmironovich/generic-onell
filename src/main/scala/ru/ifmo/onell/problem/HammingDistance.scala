package ru.ifmo.onell.problem

import scala.{specialized => sp}

import ru.ifmo.onell.util.{Helpers, OrderedSet}
import ru.ifmo.onell.{HasEvaluation, HasIncrementalEvaluation}
import ru.ifmo.onell.util.Specialization.{fitnessSpecialization => fsp}

object HammingDistance {
  implicit class Ops[@sp(fsp) F](problem: HasEvaluation[Array[Boolean], F] with HasIncrementalEvaluation[Array[Boolean], Int, F]) {
    def withHammingDistanceTracking: HasEvaluation[Array[Boolean], FAHD[F]] with HasIncrementalEvaluation[Array[Boolean], Int, FAHD[F]] =
      new Wrapper(problem)
  }

  final class FAHD[@sp(fsp) F](val fitness: F, val distance: Int)

  final class Wrapper[@sp(fsp) F](problem: HasEvaluation[Array[Boolean], F] with HasIncrementalEvaluation[Array[Boolean], Int, F])
    extends HasEvaluation[Array[Boolean], FAHD[F]] with HasIncrementalEvaluation[Array[Boolean], Int, FAHD[F]]
  {
    // Direct delegates
    override def problemSize: Int = problem.problemSize
    override def numberOfChangesForProblemSize(problemSize: Int): Int = problem.numberOfChangesForProblemSize(problemSize)
    override def sizeTypeToLong(st: Int): Long = problem.sizeTypeToLong(st)
    override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = problem.unapplyDelta(ind, delta)

    // Less direct delegates
    override def compare(lhs: FAHD[F], rhs: FAHD[F]): Int = problem.compare(lhs.fitness, rhs.fitness)
    override def isOptimalFitness(fitness: FAHD[F]): Boolean = problem.isOptimalFitness(fitness.fitness)

    override def evaluate(individual: Array[Boolean]): FAHD[F] =
      new FAHD(problem.evaluate(individual), problemSize - Helpers.countTrueBits(individual))

    override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: FAHD[F]): FAHD[F] = {
      val newDistance = currentFitness.distance - Helpers.countChanges(ind, delta)
      val newFitness = problem.applyDelta(ind, delta, currentFitness.fitness)
      new FAHD(newFitness, newDistance)
    }

    override def evaluateAssumingDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: FAHD[F]): FAHD[F] = {
      // if problem does not mutate the individual, this override won't do it as well
      val newDistance = currentFitness.distance - Helpers.countChanges(ind, delta)
      val newFitness = problem.evaluateAssumingDelta(ind, delta, currentFitness.fitness)
      new FAHD(newFitness, newDistance)
    }
  }
}
