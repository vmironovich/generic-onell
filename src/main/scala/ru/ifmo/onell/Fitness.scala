package ru.ifmo.onell

import ru.ifmo.onell.util.Specialization.{changeSpecialization => cs, fitnessSpecialization => fs}

trait Fitness[MutableIndividual, @specialized(fs) FitnessType, @specialized(cs) ChangeIndexType]
  extends HasEvaluation[MutableIndividual, FitnessType]
    with HasIncrementalEvaluation[MutableIndividual, FitnessType, ChangeIndexType]
