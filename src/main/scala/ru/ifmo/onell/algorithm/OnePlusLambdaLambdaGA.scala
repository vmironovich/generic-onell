package ru.ifmo.onell.algorithm

import java.util.concurrent.ThreadLocalRandom

import scala.annotation.tailrec
import scala.{specialized => sp}
import scala.util.chaining._

import ru.ifmo.onell.{HasDeltaOperations, HasEvaluation, HasIncrementalEvaluation, HasIndividualOperations, Optimizer}
import ru.ifmo.onell.util.Specialization.{fitnessSpecialization => fsp}
import OnePlusLambdaLambdaGA._

class OnePlusLambdaLambdaGA(lambdaTuning: Long => LambdaTuning, constantTuning: ConstantTuning = defaultTuning)
  extends Optimizer
{
  override def optimize[I, @sp(fsp) F, D](fitness: HasEvaluation[I, F] with HasIncrementalEvaluation[I, D, F])
                                         (implicit deltaOps: HasDeltaOperations[D], indOps: HasIndividualOperations[I]): Long = {
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChangesForProblemSize(problemSize)
    val lambdaP = lambdaTuning(nChanges)
    val rng = ThreadLocalRandom.current()
    val individual = indOps.createStorage(problemSize)
    val mutation, mutationBest, crossover, crossoverBest = deltaOps.createStorage(nChanges)

    @tailrec
    def runMutationsEtc(remaining: Int, baseFitness: F, bestFitness: F, change: Int): (F, Int) = {
      if (remaining == 0) (bestFitness, change) else {
        deltaOps.initializeDeltaWithGivenSize(mutation, nChanges, change, rng)
        val currFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
        if (fitness.compare(bestFitness, currFitness) < 0) {
          deltaOps.copyDelta(mutation, mutationBest)
          runMutationsEtc(remaining - 1, baseFitness, currFitness, change)
        } else {
          runMutationsEtc(remaining - 1, baseFitness, bestFitness, change)
        }
      }
    }

    @tailrec
    def runMutations(remaining: Int, baseFitness: F, expectedChange: Double): (F, Int) = {
      val change = deltaOps.initializeDeltaWithDefaultSize(mutation, nChanges, expectedChange, rng)
      if (change == 0) {
        runMutations(remaining, baseFitness, expectedChange)
      } else {
        deltaOps.copyDelta(mutation, mutationBest)
        val firstFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
        runMutationsEtc(remaining - 1, baseFitness, firstFitness, change)
      }
    }

    @tailrec
    def runCrossover(remaining: Int, soFar: Int, baseFitness: F, bestFitness: F,
                     expectedChange: Double, mutantDistance: Int): (F, Int) = {
      if (remaining == 0) (bestFitness, soFar) else {
        val size = deltaOps.initializeDeltaFromExisting(crossover, mutationBest, expectedChange, rng)
        if (size == 0) {
          // no bits from the child, skipping entirely
          runCrossover(remaining, soFar, baseFitness, bestFitness, expectedChange, mutantDistance)
        } else if (size == mutantDistance) {
          // all bits from the child, skipping evaluations but reducing budget
          runCrossover(remaining - 1, soFar, baseFitness, bestFitness, expectedChange, mutantDistance)
        } else {
          val currFitness = fitness.evaluateAssumingDelta(individual, crossover, baseFitness)
          if (fitness.compare(bestFitness, currFitness) <= 0) { // <= since we want to be able to overwrite parent
            deltaOps.copyDelta(crossover, crossoverBest)
            runCrossover(remaining - 1, soFar + 1, baseFitness, currFitness, expectedChange, mutantDistance)
          } else {
            runCrossover(remaining - 1, soFar + 1, baseFitness, bestFitness, expectedChange, mutantDistance)
          }
        }
      }
    }

    @tailrec
    def iteration(f: F, evaluationsSoFar: Long): Long = if (fitness.isOptimalFitness(f)) evaluationsSoFar else {
      val lambda = lambdaP.lambda

      val crossoverProbability = constantTuning.crossoverProbabilityQuotient * 1 / lambda

      val mutationExpectedChanges = constantTuning.mutationProbabilityQuotient * lambda
      val mutationPopSize = math.max(1, (lambda * constantTuning.firstPopulationSizeQuotient).toInt)
      val crossoverPopSize = math.max(1, (lambda * constantTuning.secondPopulationSizeQuotient).toInt)

      val (bestMutantFitness, mutantDistance) = runMutations(mutationPopSize, f, mutationExpectedChanges)
      deltaOps.copyDelta(mutationBest, crossoverBest)

      val (bestCrossFitness, crossEvs) = runCrossover(crossoverPopSize, 0, f, bestMutantFitness,
                                                      crossoverProbability * mutantDistance, mutantDistance)
      val fitnessComparison = fitness.compare(f, bestCrossFitness)
      if (fitnessComparison < 0) {
        lambdaP.notifyChildIsBetter()
      } else if (fitnessComparison > 0) {
        lambdaP.notifyChildIsWorse()
      } else {
        lambdaP.notifyChildIsEqual()
      }

      val nextFitness = if (fitnessComparison <= 0) {
        // maybe replace with silent application of delta
        fitness.applyDelta(individual, crossoverBest, f).tap(nf => assert(fitness.compare(bestCrossFitness, nf) == 0))
      } else f
      iteration(nextFitness, evaluationsSoFar + mutationPopSize + crossEvs)
    }

    indOps.initializeRandomly(individual, rng)
    iteration(fitness.evaluate(individual), 1)
  }
}

object OnePlusLambdaLambdaGA {
  trait LambdaTuning {
    def lambda: Double
    def notifyChildIsBetter(): Unit
    def notifyChildIsEqual(): Unit
    def notifyChildIsWorse(): Unit
  }

  def fixedLambda(value: Double)(size: Long): LambdaTuning = new LambdaTuning {
    override def lambda: Double = value
    override def notifyChildIsBetter(): Unit = {}
    override def notifyChildIsEqual(): Unit = {}
    override def notifyChildIsWorse(): Unit = {}
  }

  def fixedLogLambda(size: Long): LambdaTuning = new LambdaTuning {
    override def lambda: Double = 2 * math.log(size + 1)
    override def notifyChildIsBetter(): Unit = {}
    override def notifyChildIsEqual(): Unit = {}
    override def notifyChildIsWorse(): Unit = {}
  }

  def adaptiveLambda(onSuccess: Double, onFailure: Double, threshold: Long => Double)(size: Long): LambdaTuning = new LambdaTuning {
    private[this] var value = 1.0
    private[this] val maxValue = threshold(size)

    override def lambda: Double = value
    override def notifyChildIsBetter(): Unit = value = math.min(maxValue, math.max(1, value * onSuccess))
    override def notifyChildIsEqual(): Unit = notifyChildIsWorse()
    override def notifyChildIsWorse(): Unit = value = math.min(maxValue, math.max(1, value * onFailure))
  }

  def defaultAdaptiveLambda(size: Long): LambdaTuning = adaptiveLambda(OneFifthOnSuccess, OneFifthOnFailure, n => n)(size)
  def logCappedAdaptiveLambda(size: Long): LambdaTuning = adaptiveLambda(OneFifthOnSuccess, OneFifthOnFailure, n => 2 * math.log(n + 1))(size)

  case class ConstantTuning(mutationProbabilityQuotient: Double,
                            crossoverProbabilityQuotient: Double,
                            firstPopulationSizeQuotient: Double,
                            secondPopulationSizeQuotient: Double)

  final val defaultTuning = ConstantTuning(1.0, 1.0, 1.0, 1.0)
  final val OneFifthOnSuccess = 1 / 1.5
  final val OneFifthOnFailure = math.pow(1.5, 0.25)
}
