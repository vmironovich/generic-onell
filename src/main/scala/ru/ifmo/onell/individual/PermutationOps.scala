package ru.ifmo.onell.individual

import java.util.Random

import ru.ifmo.onell.HasIndividualOperations
import ru.ifmo.onell.util.Permutation

/**
  * This object contains individual operations for permutations.
  */
object PermutationOps extends HasIndividualOperations[Permutation] {
  override def createStorage(problemSize: Int): Permutation = Permutation.identity(problemSize)
  override def initializeRandomly(individual: Permutation, rng: Random): Unit = {
    val n = individual.size
    var i = 1
    while (i < n) {
      val j = rng.nextInt(i)
      if (i != j) {
        individual.swap(i, j)
      }
      i += 1
    }
  }
}
