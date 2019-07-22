package ru.ifmo.onell

import ru.ifmo.onell.util.Specialization.{fitnessSpecialization => fs}

/**
  * This trait encapsulates the knowledge for how to quickly compute the new fitness based on the old one
  * and the description of the change. The individual is assumed to be mutable.
  *
  * @tparam MutableIndividual the type of the individual (e.g. the array of booleans).
  * @tparam DeltaRepresentation the type of the representation of the change (e.g. the list of indices to flip bits at).
  * @tparam FitnessValue the type of the fitness value.
  */
trait HasIncrementalEvaluation[MutableIndividual, -DeltaRepresentation, @specialized(fs) FitnessValue] {
  /**
    * Applies the given delta to the given individual, while simultaneously recomputing the fitness.
    * @param ind the (mutable) individual.
    * @param delta the delta (the description of a change to the individual).
    * @param currentFitness the fitness of the individual before applying the change.
    * @return the new fitness after applying the change; the individual is also changed when the method returns.
    */
  def applyDelta(ind: MutableIndividual, delta: DeltaRepresentation, currentFitness: FitnessValue): FitnessValue

  /**
    * Unapplies the given delta to the given individual.
    * @param ind the (mutable) individual, which has previously experienced applying exactly the same delta.
    * @param delta the delta to be unapplied.
    */
  def unapplyDelta(ind: MutableIndividual, delta: DeltaRepresentation): Unit
}
