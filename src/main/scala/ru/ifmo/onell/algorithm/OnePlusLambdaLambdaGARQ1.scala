package ru.ifmo.onell.algorithm

import java.math.BigInteger
import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.problem.LinearRandomIntegerWeights
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}
import ru.ifmo.onell.util.lriw.BruteForceMutationCombinator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._
import scala.{specialized => sp}

class OnePlusLambdaLambdaGARQ1(lambdaTuning: Long => LambdaTuning,
                               constantTuning: ConstantTuning = defaultTuning,
                               populationRounding: PopulationSizeRounding = roundDownPopulationSize,
                               crossoverStrength: CrossoverStrength = defaultCrossoverStrength,
                               bePracticeAware: Boolean = true)
  extends Optimizer {
  override def optimize[I, @sp(fsp) F, @sp(csp) C]
  (fitness: Fitness[I, F, C],
   iterationLogger: IterationLogger[F])
  (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Long = {
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChanges
    val lambdaP = lambdaTuning(fitness.changeIndexTypeToLong(nChanges))
    val rng = Random.current()
    val individual = indOps.createStorage(problemSize)
    val mutation, mutationBest, crossover, crossoverBest = deltaOps.createStorage(nChanges)
    val aux = new Aux[F]

    @tailrec
    def initMutation(expectedChange: Double): Int = {
      val change = deltaOps.initializeDeltaWithDefaultSize(mutation, nChanges, expectedChange, rng)
      if (change == 0 && bePracticeAware) initMutation(expectedChange) else change
    }

    @tailrec
    def runMutationsEtc(remaining: Int, baseFitness: F, change: Int, bestFitness: F, badProb: Double, mutMap: mutable.HashMap[F, Double]): F = {
      if (remaining == 0) {
        bestFitness
      } else {
        deltaOps.initializeDeltaWithGivenSize(mutation, nChanges, change, rng)
        val currFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
        val currBadProb = mutMap(currFitness)

        if (currBadProb < badProb || ((currBadProb - badProb).abs < 1e-6 && fitness.compare(bestFitness, currFitness) < 0)) {
          mutationBest.copyFrom(mutation)
          runMutationsEtc(remaining - 1, baseFitness, change, currFitness, currBadProb, mutMap)
        } else {
          runMutationsEtc(remaining - 1, baseFitness, change, bestFitness, badProb, mutMap)
        }
      }
    }

    def runMutations(remaining: Int, baseFitness: F, change: Int, mutMap: mutable.HashMap[F, Double]): F = {
      mutationBest.copyFrom(mutation)
      val currentFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
      val badProb = mutMap(currentFitness)
      runMutationsEtc(remaining - 1, baseFitness, change, currentFitness, badProb, mutMap)
    }

    @tailrec
    def runPracticeAwareCrossover(remaining: Int, baseFitness: F, expectedChange: Double, mutantDistance: Int, result: Aux[F]): Unit = {
      if (remaining > 0) {
        val size = deltaOps.initializeDeltaFromExisting(crossover, mutationBest, expectedChange, rng)
        if (size == 0) {
          // no bits from the child, skipping entirely
          runPracticeAwareCrossover(remaining, baseFitness, expectedChange, mutantDistance, result)
        } else {
          if (size != mutantDistance) {
            // if not all bits from the child, we shall evaluate the offspring, and if it is better, update the best
            val currFitness = fitness.evaluateAssumingDelta(individual, crossover, baseFitness)
            aux.incrementCalls()
            if (fitness.compare(aux.fitness, currFitness) <= 0) { // <= since we want to be able to overwrite parent
              crossoverBest.copyFrom(crossover)
              aux.fitness = currFitness
            }
          }
          runPracticeAwareCrossover(remaining - 1, baseFitness, expectedChange, mutantDistance, result)
        }
      }
    }

    def fitnessFromDistance(distance: Int, baseFitness: F, mutantDistance: Int, mutantFitness: F): F = {
      if (distance == 0)
        baseFitness
      else if (distance == mutantDistance)
        mutantFitness
      else
        fitness.evaluateAssumingDelta(individual, crossover, baseFitness)
    }

    @tailrec
    def runPracticeUnawareCrossoverImpl(remaining: Int, baseFitness: F, mutantFitness: F, bestFitness: F,
                                        expectedChange: Double, mutantDistance: Int): F = {
      if (remaining == 0) bestFitness else {
        val size = deltaOps.initializeDeltaFromExisting(crossover, mutationBest, expectedChange, rng)
        val newFitness = fitnessFromDistance(size, baseFitness, mutantDistance, mutantFitness)
        if (fitness.compare(bestFitness, newFitness) < 0) {
          crossoverBest.copyFrom(crossover)
          runPracticeUnawareCrossoverImpl(remaining - 1, baseFitness, mutantFitness, newFitness, expectedChange, mutantDistance)
        } else {
          runPracticeUnawareCrossoverImpl(remaining - 1, baseFitness, mutantFitness, bestFitness, expectedChange, mutantDistance)
        }
      }
    }

    def runPracticeUnawareCrossover(remaining: Int, baseFitness: F, mutantFitness: F, expectedChange: Double, mutantDistance: Int): F = {
      assert(remaining > 0)
      val size = deltaOps.initializeDeltaFromExisting(crossover, mutationBest, expectedChange, rng)
      val newFitness = fitnessFromDistance(size, baseFitness, mutantDistance, mutantFitness)
      crossoverBest.copyFrom(crossover)
      runPracticeUnawareCrossoverImpl(remaining - 1, baseFitness, mutantFitness, newFitness, expectedChange, mutantDistance)
    }

    def calculateMutations(baseFitness: F, change: Int): mutable.HashMap[F, Double] = {
      val mutMap = new mutable.HashMap[F, Double]()
      val ind = individual.asInstanceOf[Array[Boolean]]
      val lriw = fitness.asInstanceOf[LinearRandomIntegerWeights]

      val Ns = lriw.Ns(ind)
      val combinations = BruteForceMutationCombinator.compute(Ns, change)
      val fitnessOffset = baseFitness.asInstanceOf[Long] - lriw.maxWeight * change
      val big1e18 = BigInteger.valueOf(1000000000000000000L)
      for (i <- combinations.indices) {
        val fitness = (fitnessOffset + i).asInstanceOf[F]
        val pb = combinations(i)(0)
        val pg = combinations(i)(1)
        val sum = pb.add(pg)
        if (sum.signum() > 0) {
          mutMap.put(fitness, pb.multiply(big1e18).divide(sum).doubleValue() / 1e18)
        }
      }
      mutMap
    }

    @tailrec
    def iteration(f: F, evaluationsSoFar: Long): Long = if (fitness.isOptimalFitness(f)) evaluationsSoFar else {

      val lambda = lambdaP.lambda(rng)

      val mutationExpectedChanges = constantTuning.mutationProbabilityQuotient * lambda
      val mutationPopSize = math.max(1, populationRounding(lambda * constantTuning.firstPopulationSizeQuotient, rng))
      val crossoverPopSize = math.max(1, populationRounding(lambda * constantTuning.secondPopulationSizeQuotient, rng))


      val mutantDistance = initMutation(mutationExpectedChanges)

      if (mutantDistance == 0) {
        assert(!bePracticeAware)
        iteration(f, evaluationsSoFar + mutationPopSize + crossoverPopSize)
      } else {

        val mutMap = calculateMutations(f, mutantDistance)
        //val probSum = mutMap.foldLeft(0.0) { case (a, (k, v)) => a + v._1 + v._2 }
        //assert((probSum - 1).abs < 1e-4)
        val bestMutantFitness = runMutations(mutationPopSize, f, mutantDistance, mutMap)

        val crossStrength = crossoverStrength(lambda, mutantDistance, constantTuning.crossoverProbabilityQuotient)
        if (bePracticeAware) {
          crossoverBest.copyFrom(mutationBest)
          aux.initialize(bestMutantFitness)
          runPracticeAwareCrossover(
            crossoverPopSize, f,
            crossStrength,
            mutantDistance, aux)
        } else {
          aux.initialize(runPracticeUnawareCrossover(crossoverPopSize, f, bestMutantFitness, crossStrength, mutantDistance))
          aux.incrementCalls(crossoverPopSize)
        }
        val bestCrossFitness = aux.fitness
        val crossEvs = aux.calls

        val fitnessComparison = fitness.compare(f, bestCrossFitness)
        if (fitnessComparison < 0) {
          lambdaP.notifyChildIsBetter()
        } else if (fitnessComparison > 0) {
          lambdaP.notifyChildIsWorse()
        } else {
          lambdaP.notifyChildIsEqual()
        }

        val iterationCost = mutationPopSize + crossEvs
        val nextFitness = if (fitnessComparison <= 0) {
          // maybe replace with silent application of delta
          fitness.applyDelta(individual, crossoverBest, f).tap(nf => assert(fitness.compare(bestCrossFitness, nf) == 0))
        } else f
        iterationLogger.logIteration(evaluationsSoFar + iterationCost, bestCrossFitness)
        iteration(nextFitness, evaluationsSoFar + iterationCost)
      }
    }

    indOps.initializeRandomly(individual, rng)
    val firstFitness = fitness.evaluate(individual)
    iterationLogger.logIteration(1, firstFitness)
    iteration(firstFitness, 1)
  }
}
