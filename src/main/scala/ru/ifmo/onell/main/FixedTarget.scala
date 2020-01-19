package ru.ifmo.onell.main

import java.util.concurrent.ThreadLocalRandom

import ru.ifmo.onell.algorithm.OnePlusOneEA
import ru.ifmo.onell.problem.OneMax
import ru.ifmo.onell.{HasIndividualOperations, IterationLogger, Main}

object FixedTarget extends Main.Module {
  override def name: String = "fixed-target"
  override def shortDescription: String = "Runs experiments about fixed-target performance"
  override def longDescription: Seq[String] = Seq(
    "Runs experiments about fixed-target performance of the (1+1) EA on OneMax.",
    "The parameters are:",
    "  --n          <int>: the problem size",
    "  --step       <int>: the target step to use when reporting the results",
    "  --fine-start <int>: the target to use to report the very few last targets",
    "  --fine-step  <int>: the fine target step to use in this case",
    "  --runs       <int>: over how many runs to average over",
  )

  override def moduleMain(args: Array[String]): Unit = {
    val n = args.getOption("--n").toInt
    val step = args.getOption("--step").toInt
    val fineStart = args.getOption("--fine-start").toInt
    val fineStep = args.getOption("--fine-step").toInt
    val runs = args.getOption("--runs").toInt

    val collectorZero, collectorMean = new FixedTargetLogger(n)
    val oneMax = new OneMax(n)

    locally {
      implicit val individualOps: HasIndividualOperations[Array[Boolean]] = ZeroBooleanOps
      (0 until runs).foreach(_ => OnePlusOneEA.PracticeUnaware.optimize(oneMax, collectorZero))
    }
    locally {
      (0 until runs).foreach(_ => OnePlusOneEA.PracticeUnaware.optimize(oneMax, collectorMean))
    }

    val harmonic = new Array[Double](n + 1)
    for (i <- 1 to n) {
      harmonic(i) = harmonic(i - 1) + 1.0 / i
    }
    val secondBoundStart = n / 2.0 + math.sqrt(n) * math.log(n)

    for (stepper <- Seq(0 to n by step, fineStart to n by fineStep)) {
      print("\\addplot+ coordinates {")
      for (i <- stepper) {
        val upper = math.max(1, math.E * n * (harmonic(n) - harmonic(n - i)))
        print(s"(${i.toDouble / n},$upper)")
      }
      println("};")
      println("\\addlegendentry{Upper bound~\\cite{practice-aware}};")

      print("\\addplot+ plot[error bars/.cd, y dir=both, y explicit] coordinates {")
      for (i <- stepper) {
        print(collectorZero.get(i, runs))
      }
      println("};")
      println("\\addlegendentry{$(1+1)$ EA from zero};")

      print("\\addplot+ coordinates {")
      val halfHarmonic = if (n % 2 == 0) harmonic(n / 2) else (harmonic(n / 2) + harmonic(n - n / 2)) / 2
      for (i <- stepper if i > secondBoundStart) {
        val upper = math.max(1, math.E * n * (halfHarmonic - harmonic(n - i)))
        print(s"(${i.toDouble / n},$upper)")
      }
      println("};")
      println("\\addlegendentry{Upper bound~\\cite{fixed-target-gecco19}};")

      print("\\addplot+ plot[error bars/.cd, y dir=both, y explicit] coordinates {")
      for (i <- stepper) {
        print(collectorMean.get(i, runs))
      }
      println("};")
      println("\\addlegendentry{$(1+1)$ EA from mean};")

      print("\\addplot+ coordinates {")
      for (i <- stepper) {
        val bound = math.max(1, math.E * n * math.log(n.toDouble / (n - i + 1)) - 2 * n * math.log(math.log(n)) - 16 * n)
        print(s"(${i.toDouble / n},$bound)")
      }
      println("};")
      println("\\addlegendentry{Lower bound~\\cite{lengler-fixed-budget}};")
      println()
    }
  }

  private class FixedTargetLogger(problemSize: Int) extends IterationLogger[Int] {
    private[this] val collector = new Array[Long](problemSize + 1)
    private[this] val collectorSq = new Array[Double](problemSize + 1)
    private[this] var lastFitness = -1

    def get(index: Int, runs: Int): String = {
      val avg = collector(index).toDouble / runs
      val std = math.sqrt((collectorSq(index) / runs - avg * avg) * runs / (runs - 1))
      s"(${index.toDouble / problemSize},$avg)+-(0,$std)"
    }

    override def logIteration(evaluations: Long, fitness: Int): Unit = {
      if (evaluations == 1) {
        for (i <- 0 to fitness) {
          collector(i) += 1
          collectorSq(i) += 1
        }
        lastFitness = fitness
      } else if (fitness > lastFitness) {
        val ev2 = evaluations.toDouble * evaluations
        for (i <- lastFitness + 1 to fitness) {
          collector(i) += evaluations
          collectorSq(i) += ev2
        }
        lastFitness = fitness
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

  private object ZeroBooleanOps extends HasIndividualOperations[Array[Boolean]] {
    override def createStorage(problemSize: Int): Array[Boolean] = new Array(problemSize)
    override def initializeRandomly(individual: Array[Boolean], rng: ThreadLocalRandom): Unit = {}
  }
}
