package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.concurrent.{Callable, Executors, TimeUnit}
import java.util.{Locale, Random}

import scala.jdk.CollectionConverters._
import scala.util.Using
import scala.Ordering.Double.TotalOrdering

import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.problem.HammingDistance._
import ru.ifmo.onell.problem.{LinearRandomIntegerWeights, OneMaxPerm}
import ru.ifmo.onell.{IterationLogger, Main}

object LambdaColorMap extends Main.Module {
  override def name: String = "lambda-color-map"

  override def shortDescription: String = "Runs experiments on the expected improvements depending on λ"

  override def longDescription: Seq[String] = Seq(
    "This module supports the following commands:",
    "  bits:li <options>: runs the experiments for random linear functions with integer weights on bit strings.",
    "        Options are:",
    "             --n            <int>: the problem size",
    "             --runs         <int>: the number of runs for each λ",
    "             --weight       <int>: the maximum allowed weight",
    "             --lambda-power <double>: the multiplicative step for λ to use",
    "             --out-prefix   <string>: the filename prefix to use",
    "  perm:om <options>: runs the experiments for OneMax on permutations.",
    "        Options are:",
    "             --n            <int>: the problem size",
    "             --runs         <int>: the number of runs for each λ",
    "             --lambda-power <double>: the multiplicative step for λ to use",
    "             --out-prefix   <string>: the filename prefix to use",
  )

  override def moduleMain(args: Array[String]): Unit = args(0) match {
    case "bits:li"      => collect3DPlots(n = args.getOption("--n").toInt,
                                          runs = args.getOption("--runs").toInt,
                                          lambdaPower = args.getOption("--lambda-power").toDouble,
                                          weight = args.getOption("--weight").toInt,
                                          filePrefix = args.getOption("--out-prefix"))
    case "perm:om"      => collect3DPlotsPerm(n = args.getOption("--n").toInt,
                                              runs = args.getOption("--runs").toInt,
                                              lambdaPower = args.getOption("--lambda-power").toDouble,
                                              file = args.getOption("--out-prefix"))
  }

  private class HammingImprovementStatistics(val size: Int) {
    private[this] val hammingCounts, hammingIncrements = new Array[Long](size + 1)

    def consume(distance: Int, evaluations: Long, increment: Long): Unit = synchronized {
      hammingCounts(distance) += evaluations
      hammingIncrements(distance) += increment
    }

    def extract(target: Array[Double]): Unit = {
      for (i <- 1 to size / 2) {
        target(i) = hammingIncrements(i).toDouble / hammingCounts(i)
      }
    }
  }

  private class HammingImprovementCollector(stats: HammingImprovementStatistics)
    extends IterationLogger[FAHD[Long]]
  {
    private[this] var lastEvaluations, lastFitness = 0L
    private[this] var lastDistance = -1

    override def logIteration(evaluations: Long, fitness: FAHD[Long]): Unit = {
      if (evaluations == 1) { // start
        lastEvaluations = 1
        lastDistance = fitness.distance
        lastFitness = fitness.fitness
      } else {
        if (fitness.fitness > lastFitness) {
          stats.consume(lastDistance, evaluations - lastEvaluations, lastDistance - fitness.distance)
          lastEvaluations = evaluations
          lastDistance = fitness.distance
          lastFitness = fitness.fitness
        }
      }
    }
  }

  private class PermImprovementCollector(stats: HammingImprovementStatistics)
    extends IterationLogger[Int]
  {
    private[this] var lastEvaluations = 0L
    private[this] var lastDistance = -1

    override def logIteration(evaluations: Long, fitness: Int): Unit = {
      val distance = stats.size - fitness
      if (evaluations == 1) { // start
        lastEvaluations = 1
        lastDistance = distance
      } else {
        if (distance < lastDistance) {
          stats.consume(lastDistance, evaluations - lastEvaluations, lastDistance - distance)
          lastEvaluations = evaluations
          lastDistance = distance
        }
      }
    }
  }

  private def to3dPlotNumber(v: Double): String = String.format(Locale.US, "%.2e", v)

  //noinspection SameParameterValue: IDEA wrongly reports `file` to have the same parameter value for interpolated arg
  private def collect3DPlots(optimizerFromLambda: Double => OnePlusLambdaLambdaGA,
                             n: Int, runs: Int, lambdaPower: Double, weight: Int, file: String): Unit = {
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    Using.resource(new PrintWriter(file + ".txt.cmp")) { out =>
      Using.resource(new PrintWriter(file + ".raw.cmp")) { raw =>
        val rng = new Random(9234352524211L)
        val maxLambda = (math.log(n) / 1.25 / math.log(lambdaPower)).toInt
        val arrays = Array.ofDim[Double](maxLambda, n / 2 + 1)

        def lambdaGenFun(lg: Int): Double = math.pow(lambdaPower, lg)

        for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
          val oll = optimizerFromLambda(lambda)
          val logger = new HammingImprovementStatistics(n)

          def newCallable(): Callable[Unit] = () => oll.optimize(
            new LinearRandomIntegerWeights(n, weight, rng.nextLong()).withHammingDistanceTracking,
            new HammingImprovementCollector(logger))

          executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
          logger.extract(arrays(lambdaGen))
          println(s"[$file]: lambda $lambda done")
        }
        raw.println(s"lambda power $lambdaPower")
        raw.println("distance offset 1")
        for (a <- arrays) {
          raw.println(a.view.slice(1, n / 2 + 1).map(to3dPlotNumber).mkString(" "))
        }
        for (dist <- 1 to n / 2) {
          val sumAcross = arrays.view.map(_ (dist)).max
          arrays.foreach(_ (dist) /= sumAcross)
        }
        out.println(s"lambda power $lambdaPower")
        out.println("distance offset 1")
        for (a <- arrays) {
          out.println(a.view.slice(1, n / 2 + 1).map(to3dPlotNumber).mkString(" "))
        }
      }
    }
    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)
  }

  private def collect3DPlots(n: Int, runs: Int, lambdaPower: Double, weight: Int, filePrefix: String): Unit = {
    val roundings = Seq(roundDownPopulationSize -> "down", roundUpPopulationSize -> "up", probabilisticPopulationSize -> "rnd")
    val crossovers = Seq(defaultCrossoverStrength -> "def", homogeneousCrossoverStrength -> "hom")
    val practices = Seq(true -> "aware", false -> "unaware")
    for ((rounding, roundingName) <- roundings) {
      for ((crossover, crossoverName) <- crossovers) {
        for ((practice, practiceName) <- practices) {
          collect3DPlots(lambda => new OnePlusLambdaLambdaGA(fixedLambda(lambda),
                                                             populationRounding = rounding,
                                                             crossoverStrength = crossover,
                                                             bePracticeAware = practice),
                         n, runs, lambdaPower, weight,
                         s"$filePrefix-$roundingName-$crossoverName-$practiceName")
        }
      }
    }
  }

  private def collect3DPlotsPerm(n: Int, runs: Int, lambdaPower: Double, file: String): Unit = {
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    Using.resource(new PrintWriter(file + ".txt.cmp")) { out =>
      Using.resource(new PrintWriter(file + ".raw.cmp")) { raw =>
        val maxLambda = (math.log(n) / 1.25 / math.log(lambdaPower)).toInt
        val arrays = Array.ofDim[Double](maxLambda, n / 2 + 1)

        def lambdaGenFun(lg: Int): Double = math.pow(lambdaPower, lg)

        for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
          val fun = new OneMaxPerm(n)
          val oll = new OnePlusLambdaLambdaGA(fixedLambda(lambda), populationRounding = probabilisticPopulationSize)
          val logger = new HammingImprovementStatistics(n)

          def newCallable(): Callable[Unit] = () => oll.optimize(fun, new PermImprovementCollector(logger))

          executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
          logger.extract(arrays(lambdaGen))
          println(s"[$file]: lambda $lambda done")
        }
        raw.println(s"lambda power $lambdaPower")
        raw.println(s"distance offset 2")
        for (a <- arrays) {
          raw.println(a.view.slice(2, n / 2 + 1).map(to3dPlotNumber).mkString(" "))
        }
        for (dist <- 1 to n / 2) {
          val sumAcross = arrays.view.map(_ (dist)).max
          arrays.foreach(_ (dist) /= sumAcross)
        }
        out.println(s"lambda power $lambdaPower")
        out.println(s"distance offset 2")
        for (a <- arrays) {
          out.println(a.view.slice(2, n / 2 + 1).map(to3dPlotNumber).mkString(" "))
        }
      }
    }
    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)
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
