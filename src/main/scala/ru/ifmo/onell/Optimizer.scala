package ru.ifmo.onell

import scala.{specialized => sp}
import ru.ifmo.onell.util.Specialization.{fitnessSpecialization => fsp, changeSpecialization => csp}

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
    * @tparam С the type of a single change of an individual.
    * @tparam Cs the type that can represent the number of changes.
    * @return the number of evaluations until an optimum is found.
    */
  def optimize[I, @sp(fsp) F, @sp(csp) С, @sp(csp) Cs](fitness: HasEvaluation[I, F] with HasIncrementalEvaluation[I, С, Cs, F])
                                (implicit deltaOps: HasDeltaOperations[С, Cs], indOps: HasIndividualOperations[I]): Long
}
