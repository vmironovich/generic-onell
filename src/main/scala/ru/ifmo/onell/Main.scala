package ru.ifmo.onell

import java.io.PrintWriter
import java.util.Random

import scala.util.Using
import scala.Ordering.Double.IeeeOrdering

import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA, RLS}
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, LinearRandomIntegerWeights, OneMax, OneMaxPerm, RandomPlanted3SAT}
import ru.ifmo.onell.util.par._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._

object Main {
  private def usage(): Nothing = {
    System.err.println("Usage: Main <bits:om:simple | bits:l2d:simple | bits:sat:simple | perm:om:simple | bits:om:tuning | bits:l2d:tuning | 3d>")
    sys.exit()
  }

  private class Context(powers: Range, nRuns: Int, nThreads: Int, outName: String) {
    private[this] val jsonPrefix = "["
    private[this] val jsonSeparator = "\n,"
    private[this] val jsonSuffix = "\n]\n"

    def run(fun: (Executor, Int) => Any): Unit = {
      Using.resource(new PrintWriter(outName)) { moreOut =>
        Using.resource(makeScheduler(moreOut)) { scheduler =>
          val multiplexer = new Multiplexer(scheduler, nRuns)
          for (p <- powers) {
            fun(multiplexer, 1 << p)
          }
        }
      }
    }

    private def makeScheduler(moreOut: PrintWriter): Executor = if (nThreads == 1) {
      new SequentialExecutor(moreOut, jsonPrefix, jsonSeparator, jsonSuffix)
    } else {
      new ParallelExecutor(moreOut, jsonPrefix, jsonSeparator, jsonSuffix, nThreads)
    }
  }

  private def bitsOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ<=n"       -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda),
      "(1+(λ,λ)) GA, λ<=2ln n"   -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1)),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3)),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5)),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7)),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9)),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new OneMax(n))
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsOneMaxTunings(context: Context): Unit = {
    val algorithms = Seq(
      "Up/Def"   -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, defaultTuning, roundUpPopulationSize, defaultCrossoverStrength),
      "Down/Def" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, defaultTuning, roundDownPopulationSize, defaultCrossoverStrength),
      "Rnd/Def"  -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, defaultTuning, probabilisticPopulationSize, defaultCrossoverStrength),
      "Up/Hom"   -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, defaultTuning, roundUpPopulationSize, homogeneousCrossoverStrength),
      "Down/Hom" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, defaultTuning, roundDownPopulationSize, homogeneousCrossoverStrength),
      "Rnd/Hom"  -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, defaultTuning, probabilisticPopulationSize, homogeneousCrossoverStrength),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new OneMax(n))
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsLinearDoubleSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ=8"        -> new OnePlusLambdaLambdaGA(fixedLambda(8)),
      "(1+(λ,λ)) GA, λ<=n"       -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5)),
    )

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new LinearRandomDoubleWeights(n, 2.0, seeder.nextLong()))
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsLinearDoubleTunings(context: Context): Unit = {
    val algorithms = Seq(
      "Up/Def"   -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, defaultTuning, roundUpPopulationSize, defaultCrossoverStrength),
      "Down/Def" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, defaultTuning, roundDownPopulationSize, defaultCrossoverStrength),
      "Rnd/Def"  -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, defaultTuning, probabilisticPopulationSize, defaultCrossoverStrength),
      "Up/Hom"   -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, defaultTuning, roundUpPopulationSize, homogeneousCrossoverStrength),
      "Down/Hom" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, defaultTuning, roundDownPopulationSize, homogeneousCrossoverStrength),
      "Rnd/Hom"  -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, defaultTuning, probabilisticPopulationSize, homogeneousCrossoverStrength),
    )

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new LinearRandomDoubleWeights(n, 2.0, seeder.nextLong()))
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ<=n"       -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda),
      "(1+(λ,λ)) GA, λ<=2ln n"   -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1)),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3)),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5)),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7)),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9)),
    )

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt, seeder.nextLong()))
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def permOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA),
      ("(1+(λ,λ)) GA, λ=10",     Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLambda(10))),
      ("(1+(λ,λ)) GA, λ=2ln n",  Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLogLambda)),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(logCappedOneFifthLambda)),
      ("(1+(λ,λ)) GA, λ<=n",     256,          new OnePlusLambdaLambdaGA(defaultOneFifthLambda)),
    )

    context.run { (scheduler, n) =>
      for ((name, maxN, alg) <- algorithms) {
        if (n <= maxN) {
          scheduler.addTask {
            val time = alg.optimize(new OneMaxPerm(n))
            s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n2":${time.toDouble / n / n}}"""
          }
        }
      }
    }
  }

  private class HammingImprovementCollector(size: Int) extends IterationLogger[LinearRandomIntegerWeights.FAHD] {
    private[this] val hammingCounts, hammingIncrements = new Array[Long](size + 1)
    private[this] var lastEvaluations, lastFitness = 0L
    private[this] var lastDistance = -1

    override def logIteration(evaluations: Long, fitness: LinearRandomIntegerWeights.FAHD): Unit = {
      if (evaluations == 1) { // start
        lastEvaluations = 1
        lastDistance = fitness.distance
        lastFitness = fitness.fitness
      } else {
        if (fitness.fitness > lastFitness) {
          hammingCounts(lastDistance) += evaluations - lastEvaluations
          hammingIncrements(lastDistance) += lastDistance - fitness.distance
          lastEvaluations = evaluations
          lastDistance = fitness.distance
          lastFitness = fitness.fitness
        }
      }
    }

    def extract(target: Array[Double]): Unit = {
      for (i <- 1 to size / 2) {
        target(i) = hammingIncrements(i).toDouble / hammingCounts(i)
      }
    }
  }

  private def collect3DPlots(weight: Int): Unit = {
    Using.resource(new PrintWriter("dump.txt")) { out =>
      val rng = new Random(9234352524211L)
      val n = 1000
      val runs = 1000
      val arrays = Array.ofDim[Double](100, n / 2 + 1)
      def lambdaGenFun(lg: Int): Double = math.pow(1.05, lg)

      for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
        val fun = new LinearRandomIntegerWeights(n, weight, rng.nextLong())
        val oll = new OnePlusLambdaLambdaGA(fixedLambda(lambda))
        val logger = new HammingImprovementCollector(n)
        for (_ <- 0 until runs) oll.optimize(fun, logger)
        logger.extract(arrays(lambdaGen))
        println(s"lambda $lambda done")
      }
      for (dist <- 1 to n / 2) {
        val sumAcross = arrays.view.map(_(dist)).max
        arrays.foreach(_(dist) /= sumAcross)
      }
      for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
        for (dist <- 1 to n / 2) {
          out.println(s"$dist $lambda ${arrays(lambdaGen)(dist)}")
        }
        out.println()
      }
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

  private def parseContext(args: Array[String]): Context = new Context(
    powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
    outName  = args.getOption("--out")
  )

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage()
    } else args(0) match {
      case "bits:om:simple"  => bitsOneMaxSimple(parseContext(args))
      case "bits:l2d:simple" => bitsLinearDoubleSimple(parseContext(args))
      case "bits:sat:simple" => bitsMaxSATSimple(parseContext(args))
      case "perm:om:simple"  => permOneMaxSimple(parseContext(args))
      case "bits:om:tuning"  => bitsOneMaxTunings(parseContext(args))
      case "bits:l2d:tuning" => bitsLinearDoubleTunings(parseContext(args))
      case "3d" => collect3DPlots(args(1).toInt)
      case _ => usage()
    }
  }
}
