package ru.ifmo.onell.algorithm

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._
import scala.{specialized => sp}

import ru.ifmo.onell._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.distribution.{BinomialDistribution, IntegerDistribution}
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}

class OnePlusLambdaLambdaGA(lambdaTuning: Long => LambdaTuning,
                            mutationStrength: MutationStrength,
                            constantTuning: ConstantTuning = defaultTuning,
                            populationRounding: PopulationSizeRounding = roundDownPopulationSize,
                            crossoverStrength: CrossoverStrength = defaultCrossoverStrength,
                            bePracticeAware: Boolean = true)
  extends Optimizer
{
  override def optimize[I, @sp(fsp) F, @sp(csp) C]
    (fitness: Fitness[I, F, C],
     iterationLogger: IterationLogger[F])
    (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Long =
  {
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChanges
    val nChangesL = fitness.changeIndexTypeToLong(nChanges)
    val lambdaP = lambdaTuning(nChangesL)
    val rng = Random.current()
    val individual = indOps.createStorage(problemSize)
    val mutation, mutationBest, crossover, crossoverBest = deltaOps.createStorage(nChanges)
    val aux = new Aux[F]

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
      deltaOps.initializeDeltaWithGivenSize(mutation, nChanges, change, rng)
      mutationBest.copyFrom(mutation)
      val currentFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
      runMutationsEtc(remaining - 1, baseFitness, change, currentFitness)
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

    @tailrec
    def iteration(f: F, evaluationsSoFar: Long): Long = if (fitness.isOptimalFitness(f)) evaluationsSoFar else {
      val lambda = lambdaP.lambda(rng)

      val mutationPopSize = math.max(1, populationRounding(lambda * constantTuning.firstPopulationSizeQuotient, rng))
      val crossoverPopSize = math.max(1, populationRounding(lambda * constantTuning.secondPopulationSizeQuotient, rng))

      val mutantDistance = mutationStrength(nChangesL, constantTuning.mutationProbabilityQuotient * lambda).sample(rng)
      if (mutantDistance == 0) {
        // TODO: the particular choice of the simulated fitness evaluation is a function on the crossover sampling strategy.
        iteration(f, evaluationsSoFar + mutationPopSize + crossoverPopSize)
      } else {
        val bestMutantFitness = runMutations(mutationPopSize, f, mutantDistance)
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

  trait MutationStrength {
    def apply(nChanges: Long, multipliedLambda: Double): IntegerDistribution
  }

  object MutationStrength {
    final val Standard: MutationStrength = (n, l) => BinomialDistribution(n, l / n)
    final val Resampling: MutationStrength = (n, l) => BinomialDistribution(n, l / n).filter(_ > 0)
    final val Shift: MutationStrength = (n, l) => BinomialDistribution(n, l / n).max(1)
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
    private[this] val theLambda = 2 * math.log(size + 1)
    override def lambda(rng: Random): Double = theLambda
    override def notifyChildIsBetter(): Unit = {}
    override def notifyChildIsEqual(): Unit = {}
    override def notifyChildIsWorse(): Unit = {}
  }

  def fixedLogTowerLambda(size: Long): LambdaTuning = new LambdaTuning {
    private val logN = math.log(size)
    private val logLogN = math.log(logN)
    private val theLambda = math.sqrt(logN * logLogN / math.log(logLogN))
    override def lambda(rng: Random): Double = theLambda
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

    def incrementCalls(howMuch: Int): Unit = {
      calls += howMuch
    }
  }
}
