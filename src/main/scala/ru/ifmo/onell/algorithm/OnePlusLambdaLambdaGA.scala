package ru.ifmo.onell.algorithm

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._
import scala.{specialized => sp}

import ru.ifmo.onell._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.distribution.{BinomialDistribution, IntegerDistribution}
import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}

class OnePlusLambdaLambdaGA(lambdaTuning: Long => LambdaTuning,
                            mutationStrength: MutationStrength,
                            crossoverStrength: CrossoverStrength,
                            goodMutantStrategy: GoodMutantStrategy,
                            constantTuning: ConstantTuning = defaultTuning,
                            populationRounding: PopulationSizeRounding = roundDownPopulationSize)
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
        deltaOps.initializeDelta(mutation, nChanges, change, rng)
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
      deltaOps.initializeDelta(mutation, nChanges, change, rng)
      mutationBest.copyFrom(mutation)
      val currentFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
      runMutationsEtc(remaining - 1, baseFitness, change, currentFitness)
    }

    def updateOnCrossover(result: Aux[F], currFitness: F, source: OrderedSet[C], testedQueries: Int): Unit = {
      if (testedQueries == 0 || fitness.compare(currFitness, result.fitness) > 0) {
        // A special hack: if source is crossoverBest, this means to clear crossoverBest.
        if (crossoverBest == source)
          crossoverBest.clear()
        else
          crossoverBest.copyFrom(source)
        result.fitness = currFitness
      }
    }

    def runCrossover(remaining: Int, baseFitness: F, mutantFitness: F, mutantDistance: Int,
                     distribution: IntegerDistribution, result: Aux[F]): Int = {
      var triedQueries, testedQueries = 0
      while (triedQueries < remaining) {
        val distance = distribution.sample(rng)
        if (distance == 0) {
          //                 this means "clear": vvvvvvvvvvvvv
          updateOnCrossover(result, baseFitness, crossoverBest, testedQueries)
          testedQueries += 1
          triedQueries += 1
        } else if (distance == mutantDistance) {
          updateOnCrossover(result, mutantFitness, mutationBest, testedQueries)
          triedQueries += goodMutantStrategy.incrementForTriedQueries
          testedQueries += goodMutantStrategy.incrementForTestedQueries
        } else {
          deltaOps.initializeDeltaFromExisting(crossover, mutationBest, distance, rng)
          val newFitness = fitness.evaluateAssumingDelta(individual, crossover, baseFitness)
          updateOnCrossover(result, newFitness, crossover, testedQueries)
          triedQueries += 1
          testedQueries += 1
        }
      }
      testedQueries
    }

    @tailrec
    def iteration(f: F, evaluationsSoFar: Long): Long = if (fitness.isOptimalFitness(f)) evaluationsSoFar else {
      val lambda = lambdaP.lambda(rng)

      val mutationPopSize = math.max(1, populationRounding(lambda, rng))
      val crossoverPopSize = math.max(1, populationRounding(lambda * constantTuning.crossoverPopulationSizeQuotient, rng))

      val mutantDistance = mutationStrength(nChangesL, constantTuning.mutationProbabilityQuotient * lambda).sample(rng)
      var newEvaluations = evaluationsSoFar
      val bestChildFitness = if (mutantDistance == 0) {
        // Always simulate mutations, but skip crossovers if they don't support zero distance
        val crossoverContribution = if (crossoverStrength.isStrictlyPositive) 0 else crossoverPopSize
        newEvaluations += mutationPopSize + crossoverContribution
        crossoverBest.clear()
        f
      } else {
        val bestMutantFitness = runMutations(mutationPopSize, f, mutantDistance)
        val q = constantTuning.crossoverProbabilityQuotient
        if (goodMutantStrategy == GoodMutantStrategy.SkipCrossover && fitness.compare(bestMutantFitness, f) > 0) {
          newEvaluations += mutationPopSize
          crossoverBest.copyFrom(mutationBest)
          bestMutantFitness
        } else if (crossoverStrength.willAlwaysSampleMaximum(lambda, mutantDistance, q) && goodMutantStrategy == GoodMutantStrategy.DoNotSampleIdentical) {
          // A very special case, which would enter an infinite loop if not taken care.
          // With GoodMutantStrategy.DoNotSampleIdentical,
          // a crossover which would always sample a maximum number of bits to flip would cause an infinite loop.
          // For this reason we say specially that we don't try crossovers in this case.
          newEvaluations += mutationPopSize
          if (fitness.compare(bestMutantFitness, f) > 0) {
            crossoverBest.copyFrom(mutationBest)
            bestMutantFitness
          } else {
            crossoverBest.clear()
            f
          }
        } else {
          val crossDistribution = crossoverStrength(lambda, mutantDistance, q)
          val crossEvs = runCrossover(crossoverPopSize, f, bestMutantFitness, mutantDistance, crossDistribution, aux)

          if (goodMutantStrategy == GoodMutantStrategy.DoNotSampleIdentical || goodMutantStrategy == GoodMutantStrategy.DoNotCountIdentical) {
            if (fitness.compare(bestMutantFitness, aux.fitness) > 0) {
              aux.fitness = bestMutantFitness
              crossoverBest.copyFrom(mutationBest)
            }
          }

          newEvaluations += mutationPopSize + crossEvs
          aux.fitness
        }
      }

      val fitnessComparison = fitness.compare(f, bestChildFitness)
      if (fitnessComparison < 0) {
        lambdaP.notifyChildIsBetter()
      } else if (fitnessComparison > 0) {
        lambdaP.notifyChildIsWorse()
      } else {
        lambdaP.notifyChildIsEqual()
      }

      val nextFitness = if (fitnessComparison <= 0) {
        // maybe replace with silent application of delta
        fitness.applyDelta(individual, crossoverBest, f).tap(nf => assert(fitness.compare(bestChildFitness, nf) == 0))
      } else f
      iterationLogger.logIteration(newEvaluations, bestChildFitness)
      iteration(nextFitness, newEvaluations)
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
    def apply(lambda: Double, mutantDistance: Int, quotient: Double): IntegerDistribution
    def isStrictlyPositive: Boolean
    def willAlwaysSampleMaximum(l: Double, d: Int, q: Double): Boolean
  }

  object CrossoverStrength {
    private[this] val probEps = 1e-10

    private[this] def bL(l: Double, d: Int, q: Double): IntegerDistribution = BinomialDistribution(d, q / l)
    private[this] def bD(d: Int, q: Double): IntegerDistribution = BinomialDistribution(d, q / d)

    final val StandardL: CrossoverStrength = new CrossoverStrength {
      override def apply(l: Double, d: Int, q: Double): IntegerDistribution = bL(l, d, q)
      override def isStrictlyPositive: Boolean = false
      override def willAlwaysSampleMaximum(l: Double, d: Int, q: Double): Boolean = q / l >= 1 - probEps
    }
    final val StandardD: CrossoverStrength = new CrossoverStrength {
      override def apply(l: Double, d: Int, q: Double): IntegerDistribution = bD(d, q)
      override def isStrictlyPositive: Boolean = false
      override def willAlwaysSampleMaximum(l: Double, d: Int, q: Double): Boolean = q / d >= 1 - probEps
    }
    final val ResamplingL: CrossoverStrength = new CrossoverStrength {
      override def apply(l: Double, d: Int, q: Double): IntegerDistribution = bL(l, d, q).filter(_ > 0)
      override def isStrictlyPositive: Boolean = true
      override def willAlwaysSampleMaximum(l: Double, d: Int, q: Double): Boolean = q / l >= 1 - probEps || d == 1
    }
    final val ResamplingD: CrossoverStrength = new CrossoverStrength {
      override def apply(l: Double, d: Int, q: Double): IntegerDistribution = bD(d, q).filter(_ > 0)
      override def isStrictlyPositive: Boolean = true
      override def willAlwaysSampleMaximum(l: Double, d: Int, q: Double): Boolean = q / d >= 1 - probEps || d == 1
    }
    final val ShiftL: CrossoverStrength = new CrossoverStrength {
      override def apply(l: Double, d: Int, q: Double): IntegerDistribution = bL(l, d, q).max(1)
      override def isStrictlyPositive: Boolean = true
      override def willAlwaysSampleMaximum(l: Double, d: Int, q: Double): Boolean = q / l >= 1 - probEps || d == 1
    }
    final val ShiftD: CrossoverStrength = new CrossoverStrength {
      override def apply(l: Double, d: Int, q: Double): IntegerDistribution = bD(d, q).max(1)
      override def isStrictlyPositive: Boolean = true
      override def willAlwaysSampleMaximum(l: Double, d: Int, q: Double): Boolean = q / d >= 1 - probEps || d == 1
    }
  }

  sealed trait GoodMutantStrategy {
    def incrementForTriedQueries: Int
    def incrementForTestedQueries: Int
  }
  object GoodMutantStrategy {
    case object Ignore extends GoodMutantStrategy {
      override def incrementForTestedQueries: Int = 1
      override def incrementForTriedQueries: Int = 1
    }
    case object SkipCrossover extends GoodMutantStrategy {
      override def incrementForTestedQueries: Int = 1
      override def incrementForTriedQueries: Int = 1
    }
    case object DoNotCountIdentical extends GoodMutantStrategy {
      override def incrementForTestedQueries: Int = 0
      override def incrementForTriedQueries: Int = 1
    }
    case object DoNotSampleIdentical extends GoodMutantStrategy {
      override def incrementForTestedQueries: Int = 0
      override def incrementForTriedQueries: Int = 0
    }
  }

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
                            crossoverPopulationSizeQuotient: Double)

  final val defaultTuning = ConstantTuning(1.0, 1.0, 1.0)
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
  }
}
