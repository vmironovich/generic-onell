package ru.ifmo.onell.algorithm

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.{specialized => sp}

import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}
import ru.ifmo.onell._

/**
  * This is an implementation of the "implementation-aware" (1+1) EA, which restarts mutation if zero bits are flipped.
  *
  * For mutation it uses the representation-dependent default mutation rate, which amounts to =1 change in expectation.
  */
object OnePlusOneEA extends Optimizer {
  final def optimize[I, @sp(fsp) F, @sp(csp) C]
    (fitness: Fitness[I, F, C],
     iterationLogger: IterationLogger[F])
    (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Long =
  {
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChangesForProblemSize(problemSize)
    val individual = indOps.createStorage(problemSize)
    val delta = deltaOps.createStorage(nChanges)
    val rng = Random.current()

    @tailrec
    def iterate(f: F, soFar: Long): Long = if (fitness.isOptimalFitness(f)) soFar else {
      val sz = deltaOps.initializeDeltaWithDefaultSize(delta, nChanges, 1, rng)
      if (sz == 0) iterate(f, soFar) else {
        val newF = fitness.applyDelta(individual, delta, f)
        val comparison = fitness.compare(f, newF)
        iterationLogger.logIteration(soFar + 1, newF)
        if (comparison <= 0) {
          iterate(newF, soFar + 1)
        } else {
          fitness.unapplyDelta(individual, delta)
          iterate(f, soFar + 1)
        }
      }
    }

    indOps.initializeRandomly(individual, rng)
    val firstFitness =fitness.evaluate(individual)
    iterationLogger.logIteration(1, firstFitness)
    iterate(firstFitness, 1)
  }
}
