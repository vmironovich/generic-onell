package ru.ifmo.onell.main

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import scala.jdk.CollectionConverters._
import scala.util.Using

import ru.ifmo.onell.{HasIndividualOperations, Main}
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA}
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, OneMax, OneMaxPerm, RandomPlanted3SAT}
import ru.ifmo.onell.util.par.{Executor, Multiplexer, ParallelExecutor, SequentialExecutor}

object RunningTimes extends Main.Module {
  override def name: String = "runtime"

  override def shortDescription: String = "Runs experiments on expected running times of algorithms on problems"

  override def longDescription: Seq[String] = Seq(
    "The following commands run experiments for problems on bit strings:",
    "  bits:om         <context>: for OneMax",
    "  bits:om:99      <context>: for OneMax starting at the distance of sqrt(n) from the end",
    "  bits:l2d        <context>: for linear functions with random weights from [1;2]",
    "  bits:sat        <context>: for the MAX-SAT problem with logarithmic density",
    "  bits:sat:99     <context>: same but starting at the distance of sqrt(n) from the end",
    "  bits:om:tuning  <context>: for OneMax with various tuning choices for the (1+(λ,λ)) GA",
    "  bits:l2d:tuning <context>: same for linear functions with random weights from [1;2]",
    "  bits:l5d:tuning <context>: same for linear functions with random weights from [1;5]",
    "  bits:sat:tuning <context>: same for the MAX-SAT problem with logarithmic density",
    "  bits:om:tuning* <context> <file1>,<file2>,...: for OneMax with various tuning choices",
    "                                                 for the (1+(λ,λ)) GA with constants tuned by irace",
    "The following commands run experiments for problems on permutations:",
    "  perm:om         <context>: for the permutation flavour of OneMax",
    "The <context> arguments, all mandatory, are:",
    "  --from     <int>: the minimum power of two for the problem size",
    "  --to       <int>: the maximum power of two for the problem size",
    "  --runs     <int>: the number of independent runs for each configuration",
    "  --threads  <int>: the number of threads to use (less than one: all available threads)",
    "  --out <filename>: the name of the JSON file to save the results"
  )

  override def moduleMain(args: Array[String]): Unit = args(0) match {
    case "bits:om"     => bitsOneMaxSimple(parseContext(args))
    case "bits:om:99"  => bitsOneMaxAlmostOptimal(parseContext(args))
    case "bits:l2d"    => bitsLinearDoubleSimple(parseContext(args))
    case "bits:sat"    => bitsMaxSATSimple(parseContext(args))
    case "bits:sat:99" => bitsMaxSATAlmostOptimal(parseContext(args))
    case "perm:om"     => permOneMaxSimple(parseContext(args))
    case "bits:om:tuning"  => bitsOneMaxAllTuningChoices(parseContext(args))
    case "bits:l2d:tuning" => bitsLinearDoubleTunings(parseContext(args), 2.0)
    case "bits:l5d:tuning" => bitsLinearDoubleTunings(parseContext(args), 5.0)
    case "bits:sat:tuning" => bitsMaxSatTunings(parseContext(args))
    case "bits:om:tuning*" => bitsOneMaxIRacedTuningChoices(parseContext(args), args.getOption("--files"))
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
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ=6" -> new OnePlusLambdaLambdaGA(fixedLambda(6), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(fixedLambda(8), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ=10" -> new OnePlusLambdaLambdaGA(fixedLambda(10), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ=12" -> new OnePlusLambdaLambdaGA(fixedLambda(12), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ=fixed optimal" -> new OnePlusLambdaLambdaGA(fixedLogTowerLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
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

  private def bitsOneMaxAlmostOptimal(context: Context): Unit = {
    implicit val almostOptimalBitStringOps: HasIndividualOperations[Array[Boolean]] = StartFromSqrtN

    val algorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new OneMax(n))(indOps = almostOptimalBitStringOps, deltaOps = implicitly)
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private val tuningChoices = {
    val lambdaStrategies = Seq("λ=8" -> fixedLambda(8) _,
                               "λ<=n" -> defaultOneFifthLambda _,
                               "λ<=log n" -> logCappedOneFifthLambda _)
    val mutationStrengths = Seq("standard" -> MutationStrength.Standard,
                                "shift" -> MutationStrength.Shift,
                                "resampling" -> MutationStrength.Resampling)
    val crossoverStrengths = Seq("standard on lambda" -> CrossoverStrength.StandardL,
                                 "standard on distance" -> CrossoverStrength.StandardD,
                                 "shift on lambda" -> CrossoverStrength.ShiftL,
                                 "shift on distance" -> CrossoverStrength.ShiftD,
                                 "resampling on lambda" -> CrossoverStrength.ResamplingL,
                                 "resampling on distance" -> CrossoverStrength.ResamplingD)
    val goodMutantStrategies = Seq("ignore" -> GoodMutantStrategy.Ignore,
                                   "skip crossover" -> GoodMutantStrategy.SkipCrossover,
                                   "do not count identical" -> GoodMutantStrategy.DoNotCountIdentical,
                                   "do not sample identical" -> GoodMutantStrategy.DoNotSampleIdentical)
    val populationSizeRoundings = Seq("round down" -> roundDownPopulationSize,
                                      "round up" -> roundUpPopulationSize,
                                      "probabilistic" -> probabilisticPopulationSize)
    for {
      (l, lambdaStrategy) <- lambdaStrategies
      (m, mutationStrength) <- mutationStrengths
      (c, crossoverStrength) <- crossoverStrengths
      (g, goodMutantStrategy) <- goodMutantStrategies
      (r, rounding) <- populationSizeRoundings
    } yield {
      val jsonNamePart = s""""lambda":"$l","mutation":"$m","crossover":"$c","good mutant":"$g","rounding":"$r""""
      val algGenerator = () => new OnePlusLambdaLambdaGA(lambdaStrategy, mutationStrength, crossoverStrength,
                                                         goodMutantStrategy, populationRounding = rounding)
      jsonNamePart -> algGenerator
    }
  }

  private def bitsOneMaxAllTuningChoices(context: Context): Unit = {
    context.run { (scheduler, n) =>
      for ((jsonName, algGenerator) <- tuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(new OneMax(n))
          s"""{"n":$n,"irace":0,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsOneMaxIRacedTuningChoices(context: Context, fileList: String): Unit = {
    val allLines = fileList
      .split(',')
      .flatMap(filename => Files
        .readAllLines(Paths.get(filename))
        .asScala
        .filter(_.nonEmpty)
        .toIndexedSeq)
    context.run { (scheduler, n) =>
      for (line <- allLines) {
        scheduler addTask {
          val args = line.split(" ").filter(_.nonEmpty)
          val name = IRaceClient.parseOptimizerJson("oll", args)
          val algorithm = IRaceClient.parseOptimizer("oll", args)
          val time = algorithm.optimize(new OneMax(n))
          s"""{"n":$n,"irace":1,$name,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsLinearDoubleSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(fixedLambda(8), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
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

  private def bitsLinearDoubleTunings(context: Context, maxWeight: Double): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((jsonName, algGenerator) <- tuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(new LinearRandomDoubleWeights(n, maxWeight, seeder.nextLong()))
          s"""{"n":$n,"irace":0,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical),
    )

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          val consumed = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$consumed}"""
        }
      }
    }
  }

  private def bitsMaxSatTunings(context: Context): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for ((jsonName, algGenerator) <- tuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          s"""{"n":$n,"irace":0,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATAlmostOptimal(context: Context): Unit = {
    implicit val almostOptimalBitStringOps: HasIndividualOperations[Array[Boolean]] = StartFromSqrtN

    val algorithms = Seq(
      ("RLS", Int.MaxValue, OnePlusOneEA.RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.Resampling),
      ("(1+(λ,λ)) GA, λ<=n", 16384, new OnePlusLambdaLambdaGA(defaultOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ~pow(2.1)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.1), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ~pow(2.3)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.3), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ~pow(2.5)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.5), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ~pow(2.7)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.7), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ~pow(2.9)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.9), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
    )

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, limit, alg) <- algorithms) {
        if (n <= limit) {
          scheduler addTask {
            val time = alg.optimize(new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt,
                                                          RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
            s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
          }
        }
      }
    }
  }

  private def permOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, OnePlusOneEA.RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.Resampling),
      ("(1+(λ,λ)) GA, λ=10", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLambda(10), MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLogLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
      ("(1+(λ,λ)) GA, λ<=n", 256, new OnePlusLambdaLambdaGA(defaultOneFifthLambda, MutationStrength.Resampling, CrossoverStrength.ResamplingL, GoodMutantStrategy.DoNotCountIdentical)),
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

  private implicit class Options(val args: Array[String]) extends AnyVal {
    def getOption(option: String): String = {
      val index = args.indexOf(option)
      if (index < 0) throw new IllegalArgumentException(s"No option '$option' is given")
      if (index + 1 == args.length) throw new IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
    }
  }

  private object StartFromSqrtN extends HasIndividualOperations[Array[Boolean]] {
    override def createStorage(problemSize: Int): Array[Boolean] = new Array(problemSize)
    override def initializeRandomly(individual: Array[Boolean], rng: ThreadLocalRandom): Unit = {
      val distance = math.sqrt(individual.length).toInt
      var i = individual.length
      while (i > 0) {
        i -= 1
        individual(i) = rng.nextInt(distance) != 0
      }
    }
  }

  private def parseContext(args: Array[String]): Context = new Context(
    powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
    outName  = args.getOption("--out")
  )
}
