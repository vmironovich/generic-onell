package ru.ifmo.onell

import ru.ifmo.onell.util.Specialization.fitnessSpecialization

trait ImprovementLogger[@specialized(fitnessSpecialization) F] {
  def logImprovement(evaluations: Long, oldFitness: F, newFitness: F): Unit
}

object ImprovementLogger {
  def dummyLogger[@specialized(fitnessSpecialization) F]: ImprovementLogger[F] = (_: Long, _: F, _: F) => {}
}
