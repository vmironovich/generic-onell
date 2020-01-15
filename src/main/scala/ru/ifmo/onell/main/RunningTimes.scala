package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import scala.util.Using

import ru.ifmo.onell.{HasIndividualOperations, Main}
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA, RLS}
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
    "  bits:om:tuning  <context>: for OneMax with various tuning choices for the (1+(λ,λ)) GA",
    "  bits:l2d:tuning <context>: same for linear functions with random weights from [1;2]",
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
    case "bits:om"    => bitsOneMaxSimple(parseContext(args))
    case "bits:om:99" => bitsOneMaxAlmostOptimal(parseContext(args))
    case "bits:l2d"   => bitsLinearDoubleSimple(parseContext(args))
    case "bits:sat"   => bitsMaxSATSimple(parseContext(args))
    case "perm:om"    => permOneMaxSimple(parseContext(args))
    case "bits:om:tuning"  => bitsOneMaxTunings(parseContext(args))
    case "bits:l2d:tuning" => bitsLinearDoubleTunings(parseContext(args))
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

  private def choices(lt: Long => LambdaTuning): Seq[(String, OnePlusLambdaLambdaGA)] = Seq(
    "Up/Def" -> new OnePlusLambdaLambdaGA(lt, defaultTuning, roundUpPopulationSize, defaultCrossoverStrength),
    "Down/Def" -> new OnePlusLambdaLambdaGA(lt, defaultTuning, roundDownPopulationSize, defaultCrossoverStrength),
    "Rnd/Def" -> new OnePlusLambdaLambdaGA(lt, defaultTuning, probabilisticPopulationSize, defaultCrossoverStrength),
    "Up/Hom" -> new OnePlusLambdaLambdaGA(lt, defaultTuning, roundUpPopulationSize, homogeneousCrossoverStrength),
    "Down/Hom" -> new OnePlusLambdaLambdaGA(lt, defaultTuning, roundDownPopulationSize, homogeneousCrossoverStrength),
    "Rnd/Hom" -> new OnePlusLambdaLambdaGA(lt, defaultTuning, probabilisticPopulationSize, homogeneousCrossoverStrength),
  )

  private def bitsOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA.PracticeAware,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda),
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

  private def bitsOneMaxAlmostOptimal(context: Context): Unit = {
    implicit val almostOptimalBitStringOps: HasIndividualOperations[Array[Boolean]] = new HasIndividualOperations[Array[Boolean]] {
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

    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA.PracticeAware,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1)),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3)),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5)),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7)),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9)),
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

  private def bitsOneMaxTunings(context: Context): Unit = {
    val algorithms = choices(defaultOneFifthLambda)
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
      "(1+1) EA" -> OnePlusOneEA.PracticeAware,
      "(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(fixedLambda(8)),
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda),
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
    val algorithms = choices(logCappedOneFifthLambda)
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
      "(1+1) EA" -> OnePlusOneEA.PracticeAware,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda),
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
          val time = alg.optimize(new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt,
                                                        RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def permOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.PracticeAware),
      ("(1+(λ,λ)) GA, λ=10", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLambda(10))),
      ("(1+(λ,λ)) GA, λ=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLogLambda)),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(logCappedOneFifthLambda)),
      ("(1+(λ,λ)) GA, λ<=n", 256, new OnePlusLambdaLambdaGA(defaultOneFifthLambda)),
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

  private def parseContext(args: Array[String]): Context = new Context(
    powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
    outName  = args.getOption("--out")
  )
}
