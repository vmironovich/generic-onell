package ru.ifmo.onell

import ru.ifmo.onell.util.Specialization

/**
  * This trait encapsulates fitness evaluation.
  *
  * @tparam Individual the type of an individual.
  * @tparam Fitness the type of fitness values.
  */
trait HasEvaluation[-Individual, @specialized(Specialization.fitnessSpecialization) Fitness] {
  /**
    * Evaluates the given individual.
    * @param individual the individual.
    * @return the fitness.
    */
  def evaluate(individual: Individual): Fitness

  /**
    * Compares the given fitness values.
    * @param lhs the left-hand-side fitness.
    * @param rhs the right-hand-side fitness.
    * @return zero if fitness values are equal; negative if `lhs` is worse; positive if `lhs` is greater.
    */
  def compare(lhs: Fitness, rhs: Fitness): Int

  /**
    * Returns the problem size.
    * @return the problem size.
    */
  def problemSize: Int

  /**
    * Tests whether the given fitness is optimal.
    * @param fitness the fitness value.
    * @return `true` if the fitness is optimal, `false` otherwise.
    */
  def isOptimalFitness(fitness: Fitness): Boolean
}
