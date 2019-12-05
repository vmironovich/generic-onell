package ru.ifmo.onell

import java.io.PrintWriter
import java.util.concurrent.{Executors, TimeUnit}

import scala.collection.parallel.CollectionConverters._
import scala.util.Using

import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA, RLS}
import ru.ifmo.onell.problem.{LinearRandomWeights, OneMax, OneMaxPerm}

object Main {
  private def usage(): Nothing = {
    System.err.println("Usage: Main <bits:om:simple | bits:l2:simple | perm:om:simple>")
    sys.exit()
  }

  private trait Scheduler extends AutoCloseable {
    def addTask(fun: => String): Unit
  }

  private class SingleThreaded(pw: PrintWriter, firstLine: String, lastLine: String) extends Scheduler {
    pw.println(firstLine)
    override def addTask(fun: => String): Unit = {
      val line = fun
      pw.println(line)
      pw.flush()
      println(line)
    }
    override def close(): Unit = pw.println(lastLine)
  }

  private class MultiThreaded(pw: PrintWriter, firstLine: String, lastLine: String) extends Scheduler {
    private[this] val lock = new AnyRef
    private val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    pw.println(firstLine)
    override def addTask(fun: => String): Unit = pool.execute(() => {
      val line = fun
      lock synchronized {
        pw.println(line)
        pw.flush()
        println(line)
      }
    })
    override def close(): Unit = {
      pool.shutdown()
      pool.awaitTermination(365, TimeUnit.DAYS)
      pw.println(lastLine)
    }
  }

  private def makeScheduler(moreOut: PrintWriter, parallel: Boolean): Scheduler = if (parallel) {
    new MultiThreaded(moreOut, "[{}", "]")
  } else {
    new SingleThreaded(moreOut, "[{}", "]")
  }

  private def bitsOneMaxSimple(powers: Range, nRuns: Int, parallel: Boolean, outName: String): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.1)),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.5)),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.7)),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.9)),
    )

    Using.resource(new PrintWriter(outName)) { moreOut =>
      Using.resource(makeScheduler(moreOut, parallel)) { scheduler =>
        for (p <- powers; n = 1 << p) {
          for ((name, alg) <- algorithms) {
            for (_ <- 0 until nRuns) {
              scheduler addTask {
                val time = alg.optimize(new OneMax(n))
                s""",{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
              }
            }
          }
        }
      }
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
      Using.resource(makeScheduler(moreOut, parallel)) { scheduler =>
        for (p <- powers; n = 1 << p) {
          for ((name, maxN, alg) <- algorithms) {
            if (n <= maxN) {
              for (_ <- 0 until nRuns) {
                scheduler.addTask {
                  val time = alg.optimize(new OneMaxPerm(n))
                  s""",{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n2":${time.toDouble / n / n}}"""
                }
              }
            }
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

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage()
    } else args(0) match {
      case "bits:om:simple" =>
        bitsOneMaxSimple(powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
                         nRuns    = args.getOption("--runs").toInt,
                         parallel = args.contains("--par"),
                         outName  = args.getOption("--out"))
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
