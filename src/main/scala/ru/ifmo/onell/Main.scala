package ru.ifmo.onell

import scala.collection.parallel.CollectionConverters._

import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA, RLS}
import ru.ifmo.onell.problem.{OneMax, OneMaxPerm}

object Main {
  private def usage(): Nothing = {
    System.err.println("Usage: Main <bits:om:simple | perm:om:simple>")
    sys.exit()
  }

  private def bitsOneMaxSimple(): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda),
      "(1+(λ,λ)) GA log" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda),
    )
    for (n <- 1000 to 10000 by 1000) {
      println(s"n = $n:")
      val oneMax = new OneMax(n)
      for ((name, alg) <- algorithms) {
        val runs = IndexedSeq.fill(100)(alg.optimize(oneMax)).sorted
        println(f"  $name%16s: ${runs.sum.toDouble / runs.size}%9.2f (min = ${runs.head}%6d, max = ${runs.last}%6d)")
      }
      println()
    }
  }

  private def permOneMaxSimple(nRuns: Int, parallel: Boolean): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA/10" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLambda(10)),
      "(1+(λ,λ)) GA log" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda),
      "(1+(λ,λ)) GA" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda),
    )
    println("[")
    for (p <- 16 to 16; n = 1 << p) {
      val oneMaxPerm = new OneMaxPerm(n)
      for ((name, alg) <- algorithms) {
        if (name != algorithms.last._1 || n <= 256) {
          def oneRun(): Unit = {
            val time = alg.optimize(oneMaxPerm)
            synchronized{
              println(s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n2":${time.toDouble / n / n}},""")
            }
          }

          if (parallel) {
            for (_ <- (0 until nRuns).par) oneRun()
          } else {
            for (_ <- 0 until nRuns) oneRun()
          }
        }
      }
    }
    println("{}]")
  }

  private implicit class Options(val args: Array[String]) extends AnyVal {
    def getOption(option: String): String = {
      val index = args.indexOf(option)
      if (index < 0) throw new IllegalArgumentException(s"No option '$option' is given")
      if (index + 1 == args.length) throw new IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage()
    } else args(0) match {
      case "bits:om:simple" => bitsOneMaxSimple()
      case "perm:om:simple" => permOneMaxSimple(args.getOption("--runs").toInt, args.contains("--par"))
      case _ => usage()
    }
  }
}
