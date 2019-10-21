package ru.ifmo.onell

import java.io.PrintWriter

import scala.collection.parallel.CollectionConverters._
import scala.util.Using

import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA, RLS}
import ru.ifmo.onell.problem.{LinearRandomWeights, OneMax, OneMaxPerm}

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

  private def bitsLinearSimple(): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLambda(8)),
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda)
    )

    for ((name, alg) <- algorithms) {
      print("\\addplot coordinates{")
      for (n <- (4 to 11).map(i => math.pow(10, i / 2.0).toInt)) {
        val runs = (0 until 100).par.map(_ => alg.optimize(new LinearRandomWeights(n, 2))).seq
        val result = runs.sum.toDouble / runs.size / n
        print(s"($n,$result)")
      }
      println("};")
      println(s"\\addlegendentry{$name};")
    }
  }

  private def permOneMaxSimple(powers: Range, nRuns: Int, parallel: Boolean, outName: String): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA),
      ("(1+(λ,λ)) GA, λ=10", Int.MaxValue, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLambda(10))),
      ("(1+(λ,λ)) GA, λ=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLogLambda)),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda)),
      ("(1+(λ,λ)) GA, λ<=n", 256, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda)),
    )
    Using.resource(new PrintWriter(outName)) { moreOut =>
      println("[")
      moreOut.println("[")
      for (p <- powers; n = 1 << p) {
        val oneMaxPerm = new OneMaxPerm(n)
        for ((name, maxN, alg) <- algorithms) {
          if (n <= maxN) {
            def oneRun(): Unit = {
              val time = alg.optimize(oneMaxPerm)
              synchronized {
                val line = s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n2":${time.toDouble / n / n}},"""
                println(line)
                moreOut.println(line)
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
      moreOut.println("{}]")
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

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage()
    } else args(0) match {
      case "bits:om:simple" => bitsOneMaxSimple()
      case "bits:l2:simple" => bitsLinearSimple()
      case "perm:om:simple" =>
        permOneMaxSimple(powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
                         nRuns    = args.getOption("--runs").toInt,
                         parallel = args.contains("--par"),
                         outName  = args.getOption("--out"))
      case _ => usage()
    }
  }
}
