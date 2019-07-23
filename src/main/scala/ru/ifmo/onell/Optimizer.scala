package ru.ifmo.onell

import scala.{specialized => sp}
import ru.ifmo.onell.util.Specialization.{fitnessSpecialization => fsp}

/**
  * A common trait for optimizers.
  */
trait Optimizer {
  /**
    * Runs optimization for a given problem.
    *
    * @param fitness the problem description, including problem size, termination condition, and fitness evaluation.
    * @param deltaOps the implicit that explains how to manage representations of individual deltas.
    * @param indOps the implicit that explains how to manage representations of individuals.
    * @tparam I the type of an individual.
    * @tparam F the type of a fitness value.
    * @tparam D the type of an individual delta.
    * @return the number of evaluations until an optimum is found.
    */
  def optimize[I, @sp(fsp) F, D](fitness: HasEvaluation[I, F] with HasIncrementalEvaluation[I, D, F])
                                (implicit deltaOps: HasDeltaOperations[D], indOps: HasIndividualOperations[I]): Int
}
