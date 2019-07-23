package ru.ifmo.onell

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

  private def permOneMaxSimple(): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA/10" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLambda(10)),
      "(1+(λ,λ)) GA log" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda),
      "(1+(λ,λ)) GA" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda),
    )
    println("[")
    for (p <- 4 to 14; n = 1 << p) {
      val oneMaxPerm = new OneMaxPerm(n)
      for ((name, alg) <- algorithms) {
        if (name != algorithms.last._1 || n <= 256) {
          for (_ <- 0 to 10) {
            println(s"""{"n":$n,"algorithm":"$name","runtime":${alg.optimize(oneMaxPerm)}},""")
          }
        }
      }
    }
    println("{}]")
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage()
    } else args(0) match {
      case "bits:om:simple" => bitsOneMaxSimple()
      case "perm:om:simple" => permOneMaxSimple()
      case _ => usage()
    }
  }
}
