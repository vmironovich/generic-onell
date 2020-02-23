package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import scala.util.Using

import ru.ifmo.onell.{IterationLogger, Main}
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.problem.HammingDistance._
import ru.ifmo.onell.problem.LinearRandomIntegerWeights

object LambdaTraces extends Main.Module {
  override def name: String = "lambda-traces"

  override def shortDescription: String = "Runs experiments on how the parameter λ changes over time"

  override def longDescription: Seq[String] = Seq(
    "Runs experiments on how the parameter λ changes over time.",
    "The experiments are done for linear functions with random",
    "integer weights on bit strings. The options are:",
    "  --n             <int>: the problem size",
    "  --runs          <int>: the number of runs",
    "  --weight        <int>: the maximum weight",
    "  --out-prefix <string>: the prefix of the output files to use"
  )

  override def moduleMain(args: Array[String]): Unit = {
    collectTraces(n = args.getOption("--n").toInt,
                  runs = args.getOption("--runs").toInt,
                  weight = args.getOption("--weight").toInt,
                  filePrefix = args.getOption("--out-prefix"))
  }

  private class LoggerWithLambdaProxy
    extends IterationLogger[FAHD[Long]]
  {
    private[this] var lastLambda: Double = _
    private[this] var lastFitness: Long = -1
    private[this] val builder = new StringBuilder

    override def logIteration(evaluations: Long, fitness: FAHD[Long]): Unit = {
      if (fitness.distance != 0 && fitness.fitness >= lastFitness) {
        lastFitness = fitness.fitness
        builder.append("(").append(fitness.distance).append(",").append(lastLambda).append(")")
      }
    }

    def result(): String = {
      val result = builder.result()
      builder.clear()
      lastFitness = -1
      result
    }

    def attachedTuning(realTuning: Long => LambdaTuning)(size: Long): LambdaTuning = new LambdaTuning {
      private[this] val delegate = realTuning(size)
      override def lambda(rng: ThreadLocalRandom): Double = {
        lastLambda = delegate.lambda(rng)
        lastLambda
      }

      override def notifyChildIsBetter(): Unit = delegate.notifyChildIsBetter()
      override def notifyChildIsEqual(): Unit = delegate.notifyChildIsEqual()
      override def notifyChildIsWorse(): Unit = delegate.notifyChildIsWorse()
    }
  }

  //noinspection SameParameterValue: IDEA wrongly reports `file` to have the same parameter value for interpolated arg
  private def collectTraces(algorithm: (Long => LambdaTuning) => OnePlusLambdaLambdaGA,
                            n: Int, runs: Int, weight: Int, file: String): Unit = {
    Using.resource(new PrintWriter(file)) { out =>
      val rng = new Random(n * 234234 + runs * 912645 + weight * 213425431 + file.hashCode)
      val logger = new LoggerWithLambdaProxy
      for (_ <- 0 until runs) {
        val problem = new LinearRandomIntegerWeights(n, weight, rng.nextLong()).withHammingDistanceTracking
        val oll = algorithm(logger.attachedTuning(defaultOneFifthLambda))
        oll.optimize(problem, logger)
        out.println("\\addplot[black] coordinates {" + logger.result() + "};")
      }
    }
  }

  private def collectTraces(n: Int, runs: Int, weight: Int, filePrefix: String): Unit = {
    val roundings = Seq(roundDownPopulationSize -> "down", roundUpPopulationSize -> "up", probabilisticPopulationSize -> "rnd")
    val mutations = Seq(MutationStrength.Standard -> "std", MutationStrength.Resampling -> "res", MutationStrength.Shift -> "shf")
    val crossovers = Seq(CrossoverStrength.StandardD -> "stdD", CrossoverStrength.StandardL -> "stdL",
                         CrossoverStrength.ResamplingD -> "resD", CrossoverStrength.ResamplingL -> "resL",
                         CrossoverStrength.ShiftD -> "shfD", CrossoverStrength.ShiftL -> "shfL")
    val goodMutants = Seq(GoodMutantStrategy.Ignore -> "ignore", GoodMutantStrategy.SkipCrossover -> "skip",
                          GoodMutantStrategy.DoNotCountIdentical -> "notcount", GoodMutantStrategy.DoNotSampleIdentical -> "notsample")
    for ((rounding, roundingName) <- roundings;
         (mutation, mutationName) <- mutations;
         (crossover, crossoverName) <- crossovers;
         (goodMutant, goodMutantName) <- goodMutants) {
      collectTraces(gen => new OnePlusLambdaLambdaGA(gen,
                                                     mutationStrength = mutation,
                                                     crossoverStrength = crossover,
                                                     goodMutantStrategy = goodMutant,
                                                     populationRounding = rounding),
                    n, runs, weight,
                    s"$filePrefix-$roundingName-$mutationName-$crossoverName-$goodMutantName.tex")
    }
  }

  private implicit class Options(val args: Array[String]) extends AnyVal {
    def getOption(option: String): String = {
      val index = args.indexOf(option)
      if (index < 0) throw new IllegalArgumentException(s"No option '$option' is given")
      if (index + 1 == args.length) throw new IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
    }
  }
}
