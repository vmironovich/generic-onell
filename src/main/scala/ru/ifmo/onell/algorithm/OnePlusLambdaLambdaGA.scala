package ru.ifmo.onell.algorithm

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.{specialized => sp}
import scala.util.chaining._

import ru.ifmo.onell.{HasDeltaOperations, HasEvaluation, HasIncrementalEvaluation, HasIndividualOperations, IterationLogger, Optimizer}
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}
import OnePlusLambdaLambdaGA._

class OnePlusLambdaLambdaGA(lambdaTuning: Long => LambdaTuning,
                            constantTuning: ConstantTuning = defaultTuning,
                            populationRounding: PopulationSizeRounding = roundDownPopulationSize,
                            crossoverStrength: CrossoverStrength = defaultCrossoverStrength,
                            bePracticeAware: Boolean = true)
  extends Optimizer
{
  override def optimize[I, @sp(fsp) F, @sp(csp) C]
    (fitness: HasEvaluation[I, F] with HasIncrementalEvaluation[I, F, C],
     iterationLogger: IterationLogger[F])
    (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Long =
  {
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChangesForProblemSize(problemSize)
    val lambdaP = lambdaTuning(fitness.sizeTypeToLong(nChanges))
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
    def runMutationsEtc(remaining: Int, baseFitness: F, change: Int, bestFitness: F): F = {
      if (remaining == 0) {
        bestFitness
      } else {
        deltaOps.initializeDeltaWithGivenSize(mutation, nChanges, change, rng)
        val currFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
        if (fitness.compare(bestFitness, currFitness) < 0) {
          mutationBest.copyFrom(mutation)
          runMutationsEtc(remaining - 1, baseFitness, change, currFitness)
        } else {
          runMutationsEtc(remaining - 1, baseFitness, change, bestFitness)
        }
      }
    }

    def runMutations(remaining: Int, baseFitness: F, change: Int): F = {
      mutationBest.copyFrom(mutation)
      val currentFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
      runMutationsEtc(remaining - 1, baseFitness, change, currentFitness)
    }

    @tailrec
    def runCrossover(remaining: Int, baseFitness: F, expectedChange: Double, mutantDistance: Int, result: Aux[F]): Unit = {
      if (remaining > 0) {
        val size = deltaOps.initializeDeltaFromExisting(crossover, mutationBest, expectedChange, rng)
        if (size == 0) {
          if (bePracticeAware) {
            // no bits from the child, skipping entirely
            runCrossover(remaining, baseFitness, expectedChange, mutantDistance, result)
          } else {
            aux.incrementCalls()
            runCrossover(remaining - 1, baseFitness, expectedChange, mutantDistance, result)
          }
        } else {
          if (size != mutantDistance) {
            // if not all bits from the child, we shall evaluate the offspring, and if it is better, update the best
            val currFitness = fitness.evaluateAssumingDelta(individual, crossover, baseFitness)
            aux.incrementCalls()
            if (fitness.compare(aux.fitness, currFitness) <= 0) { // <= since we want to be able to overwrite parent
              crossoverBest.copyFrom(crossover)
              aux.fitness = currFitness
            }
          } else {
            if (!bePracticeAware) {
              aux.incrementCalls()
            }
          }
          runCrossover(remaining - 1, baseFitness, expectedChange, mutantDistance, result)
        }
      }
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
        val bestMutantFitness = runMutations(mutationPopSize, f, mutantDistance)

        crossoverBest.copyFrom(mutationBest)

        aux.initialize(bestMutantFitness)
        runCrossover(crossoverPopSize, f,
                     crossoverStrength(lambda, mutantDistance, constantTuning.crossoverProbabilityQuotient),
                     mutantDistance, aux)
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

object OnePlusLambdaLambdaGA {
  trait LambdaTuning {
    def lambda(rng: Random): Double
    def notifyChildIsBetter(): Unit
    def notifyChildIsEqual(): Unit
    def notifyChildIsWorse(): Unit
  }

  trait PopulationSizeRounding {
    def apply(fpValue: Double, rng: Random): Int
  }

  final val roundUpPopulationSize: PopulationSizeRounding = (fpValue: Double, _: Random) => math.ceil(fpValue).toInt
  final val roundDownPopulationSize: PopulationSizeRounding = (fpValue: Double, _: Random) => fpValue.toInt
  final val probabilisticPopulationSize: PopulationSizeRounding = (fpValue: Double, rng: Random) => {
    val lower = math.floor(fpValue).toInt
    val upper = math.ceil(fpValue).toInt
    if (lower == upper || rng.nextDouble() < upper - fpValue) lower else upper
  }

  trait CrossoverStrength {
    def apply(lambda: Double, mutantDistance: Int, quotient: Double): Double
  }

  final val defaultCrossoverStrength: CrossoverStrength =
    (lambda: Double, mutantDistance: Int, quotient: Double) => quotient / lambda * mutantDistance
  final val homogeneousCrossoverStrength: CrossoverStrength =
    (_: Double, _: Int, quotient: Double) => quotient

  def fixedLambda(value: Double)(size: Long): LambdaTuning = new LambdaTuning {
    override def lambda(rng: Random): Double = value
    override def notifyChildIsBetter(): Unit = {}
    override def notifyChildIsEqual(): Unit = {}
    override def notifyChildIsWorse(): Unit = {}
  }

  def fixedLogLambda(size: Long): LambdaTuning = new LambdaTuning {
    override def lambda(rng: Random): Double = 2 * math.log(size + 1)
    override def notifyChildIsBetter(): Unit = {}
    override def notifyChildIsEqual(): Unit = {}
    override def notifyChildIsWorse(): Unit = {}
  }

  def powerLawLambda(beta: Double)(size: Long): LambdaTuning = new LambdaTuning {
    private[this] val weights = collectWeightsUntilThreshold(beta, 1, size, 0, Array.newBuilder[Double])

    override def lambda(rng: Random): Double = {
      val query = weights.last * rng.nextDouble()
      val index0 = java.util.Arrays.binarySearch(weights, query)
      val index = if (index0 >= 0) index0 else -index0 - 1
      index + 1 // since index=0 corresponds to lambda=1
    }
    override def notifyChildIsBetter(): Unit = {}
    override def notifyChildIsEqual(): Unit = {}
    override def notifyChildIsWorse(): Unit = {}
  }

  def oneFifthLambda(onSuccess: Double, onFailure: Double, threshold: Long => Double)(size: Long): LambdaTuning = new LambdaTuning {
    private[this] var value = 1.0
    private[this] val maxValue = threshold(size)

    override def lambda(rng: Random): Double = value
    override def notifyChildIsBetter(): Unit = value = math.min(maxValue, math.max(1, value * onSuccess))
    override def notifyChildIsEqual(): Unit = notifyChildIsWorse()
    override def notifyChildIsWorse(): Unit = value = math.min(maxValue, math.max(1, value * onFailure))
  }

  def defaultOneFifthLambda(size: Long): LambdaTuning = oneFifthLambda(OneFifthOnSuccess, OneFifthOnFailure, n => n)(size)
  def logCappedOneFifthLambda(size: Long): LambdaTuning = oneFifthLambda(OneFifthOnSuccess, OneFifthOnFailure, n => 2 * math.log(n + 1))(size)

  case class ConstantTuning(mutationProbabilityQuotient: Double,
                            crossoverProbabilityQuotient: Double,
                            firstPopulationSizeQuotient: Double,
                            secondPopulationSizeQuotient: Double)

  final val defaultTuning = ConstantTuning(1.0, 1.0, 1.0, 1.0)
  final val OneFifthOnSuccess = 1 / 1.5
  final val OneFifthOnFailure = math.pow(1.5, 0.25)

  @tailrec
  private[this] def collectWeightsUntilThreshold(beta: Double, index: Long, size: Long, cumulative: Double,
                                                 weights: mutable.ArrayBuilder[Double]): Array[Double] = {
    val addend = cumulative + math.pow(index, -beta)
    if (index > size || addend == 0) weights.result() else {
      weights += addend
      collectWeightsUntilThreshold(beta, index + 1, size, addend, weights)
    }
  }

  private final class Aux[@sp(fsp) F] {
    var fitness: F = _
    var calls: Int = _

    def initialize(fitness: F): Unit = {
      this.fitness = fitness
      this.calls = 0
    }

    def incrementCalls(): Unit = {
      calls += 1
    }
  }
}
