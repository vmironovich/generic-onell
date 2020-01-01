package ru.ifmo.onell

import ru.ifmo.onell.util.Specialization.fitnessSpecialization

trait IterationLogger[@specialized(fitnessSpecialization) F] {
  def logIteration(evaluations: Long, fitness: F): Unit
}

object IterationLogger {
  def dummyLogger[@specialized(fitnessSpecialization) F]: IterationLogger[F] = (_: Long, _: F) => {}
}
