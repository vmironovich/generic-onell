package ru.ifmo.onell

import java.io.PrintWriter
import java.util.Random
import java.util.concurrent.{Callable, Executors, ThreadLocalRandom, TimeUnit}

import scala.util.Using
import scala.Ordering.Double.IeeeOrdering
import scala.jdk.CollectionConverters._

import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA, RLS}
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, LinearRandomIntegerWeights, OneMax, OneMaxPerm, RandomPlanted3SAT}
import ru.ifmo.onell.util.par._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.problem.HammingDistance._

object Main {
  private def usage(): Nothing = {
    System.err.println("Usage: Main <bits:om:simple | bits:l2d:simple | bits:sat:simple | perm:om:simple | bits:om:tuning | bits:l2d:tuning | bits:li:3d | bits:li:traces | perm:om:3d>")
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

  //noinspection SameParameterValue: IDEA wrongly reports `file` to have the same parameter value for interpolated arg
  private def collect3DPlots(optimizerFromLambda: Double => OnePlusLambdaLambdaGA,
                             n: Int, runs: Int, lambdaPower: Double, weight: Int, file: String): Unit = {
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    Using.resource(new PrintWriter(file + ".txt")) { out =>
      Using.resource(new PrintWriter(file + ".raw")) { raw =>
        val rng = new Random(9234352524211L)
        val maxLambda = (math.log(n) / 1.25 / math.log(lambdaPower)).toInt
        val arrays = Array.ofDim[Double](maxLambda, n / 2 + 1)

        def lambdaGenFun(lg: Int): Double = math.pow(lambdaPower, lg)

        for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
          val fun = new LinearRandomIntegerWeights(n, weight, rng.nextLong()).withHammingDistanceTracking
          val oll = optimizerFromLambda(lambda)
          val logger = new HammingImprovementStatistics(n)

          def newCallable(): Callable[Unit] = () => oll.optimize(fun, new HammingImprovementCollector(logger))

          executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
          logger.extract(arrays(lambdaGen))
          println(s"[$file]: lambda $lambda done")
        }
        for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
          for (dist <- 1 to n / 2) {
            raw.println(s"$dist $lambda ${arrays(lambdaGen)(dist)}")
          }
          raw.println()
        }
        for (dist <- 1 to n / 2) {
          val sumAcross = arrays.view.map(_ (dist)).max
          arrays.foreach(_ (dist) /= sumAcross)
        }
        for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
          for (dist <- 1 to n / 2) {
            out.println(s"$dist $lambda ${arrays(lambdaGen)(dist)}")
          }
          out.println()
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
    Using.resource(new PrintWriter(file + ".txt")) { out =>
      Using.resource(new PrintWriter(file + ".raw")) { raw =>
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
        for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
          for (dist <- 1 to n / 2) {
            raw.println(s"$dist $lambda ${arrays(lambdaGen)(dist)}")
          }
          raw.println()
        }
        for (dist <- 1 to n / 2) {
          val sumAcross = arrays.view.map(_ (dist)).max
          arrays.foreach(_ (dist) /= sumAcross)
        }
        for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
          for (dist <- 1 to n / 2) {
            out.println(s"$dist $lambda ${arrays(lambdaGen)(dist)}")
          }
          out.println()
        }
      }
    }
    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)
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
    val crossovers = Seq(defaultCrossoverStrength -> "def", homogeneousCrossoverStrength -> "hom")
    val practices = Seq(true -> "aware", false -> "unaware")
    for ((rounding, roundingName) <- roundings) {
      for ((crossover, crossoverName) <- crossovers) {
        for ((practice, practiceName) <- practices) {
          collectTraces(gen => new OnePlusLambdaLambdaGA(gen,
                                                         populationRounding = rounding,
                                                         crossoverStrength = crossover,
                                                         bePracticeAware = practice),
                         n, runs, weight,
                         s"$filePrefix-$roundingName-$crossoverName-$practiceName.tex")
        }
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
      case "bits:li:3d"      => collect3DPlots(n = args.getOption("--n").toInt,
                                               runs = args.getOption("--runs").toInt,
                                               lambdaPower = args.getOption("--lambda-power").toDouble,
                                               weight = args.getOption("--weight").toInt,
                                               filePrefix = args.getOption("--out-prefix"))
      case "bits:li:traces"  => collectTraces(n = args.getOption("--n").toInt,
                                              runs = args.getOption("--runs").toInt,
                                              weight = args.getOption("--weight").toInt,
                                              filePrefix = args.getOption("--out-prefix"))
      case "perm:om:3d"      => collect3DPlotsPerm(n = args.getOption("--n").toInt,
                                                   runs = args.getOption("--runs").toInt,
                                                   lambdaPower = args.getOption("--lambda-power").toDouble,
                                                   file = args.getOption("--out"))
      case _ => usage()
    }
  }
}
