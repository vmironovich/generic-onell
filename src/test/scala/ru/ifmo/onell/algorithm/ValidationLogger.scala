package ru.ifmo.onell.algorithm

import ru.ifmo.onell.ImprovementLogger

class ValidationLogger extends ImprovementLogger[Int] {
  private[this] var totalCalls: Long = 0

  def calls: Long = totalCalls
  override def logImprovement(evaluations: Long, oldFitness: Int, newFitness: Int): Unit = {
    assert(oldFitness < newFitness)
    totalCalls += evaluations
  }
}
