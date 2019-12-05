package ru.ifmo.onell

import java.io.PrintWriter
import java.util.concurrent.{Executors, TimeUnit}

import scala.util.Using
import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA, RLS}
import ru.ifmo.onell.problem.{LinearRandomWeights, OneMax, OneMaxPerm, RandomPlanted3SAT}

object Main {
  private def usage(): Nothing = {
    System.err.println("Usage: Main <bits:om:simple | bits:l2:simple | bits:sat:simple | perm:om:simple>")
    sys.exit()
  }

  private class Context(powers: Range, nRuns: Int, isParallel: Boolean, outName: String) {
    def run(fun: (Scheduler, Int) => Any): Unit = {
      Using.resource(new PrintWriter(outName)) { moreOut =>
        Using.resource(makeScheduler(moreOut)) { scheduler =>
          val multiplexer = new Multiplexer(scheduler, nRuns)
          for (p <- powers) {
            fun(multiplexer, 1 << p)
          }
        }
      }
    }

    private def makeScheduler(moreOut: PrintWriter): Scheduler = if (isParallel) {
      new MultiThreaded(moreOut, "[{}", "]")
    } else {
      new SingleThreaded(moreOut, "[{}", "]")
    }
  }

  private trait Scheduler extends AutoCloseable {
    def addTask(fun: => String): Unit
  }

  private class Multiplexer(base: Scheduler, times: Int) extends Scheduler {
    override def close(): Unit = {}
    override def addTask(fun: => String): Unit = {
      for (_ <- 0 until times) {
        base.addTask(fun)
      }
    }
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

  private def bitsOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.1)),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.3)),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.5)),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.7)),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.9)),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new OneMax(n))
          s""",{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsLinearSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLambda(8)),
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.5)),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new LinearRandomWeights(n, 2))
          s""",{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> RLS,
      "(1+1) EA" -> OnePlusOneEA,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.1)),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.3)),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.5)),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.7)),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.powerLawLambda(2.9)),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt))
          s""",{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def permOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA),
      ("(1+(λ,λ)) GA, λ=10", Int.MaxValue, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLambda(10))),
      ("(1+(λ,λ)) GA, λ=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.fixedLogLambda)),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.logCappedAdaptiveLambda)),
      ("(1+(λ,λ)) GA, λ<=n", 256, new OnePlusLambdaLambdaGA(OnePlusLambdaLambdaGA.defaultAdaptiveLambda)),
    )

    context.run { (scheduler, n) =>
      for ((name, maxN, alg) <- algorithms) {
        if (n <= maxN) {
          scheduler.addTask {
            val time = alg.optimize(new OneMaxPerm(n))
            s""",{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n2":${time.toDouble / n / n}}"""
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
    powers     = args.getOption("--from").toInt to args.getOption("--to").toInt,
    nRuns      = args.getOption("--runs").toInt,
    isParallel = args.contains("--par"),
    outName    = args.getOption("--out")
  )

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage()
    } else args(0) match {
      case "bits:om:simple"  => bitsOneMaxSimple(parseContext(args))
      case "bits:l2:simple"  => bitsLinearSimple(parseContext(args))
      case "bits:sat:simple" => bitsMaxSATSimple(parseContext(args))
      case "perm:om:simple"  => permOneMaxSimple(parseContext(args))
      case _ => usage()
    }
  }
}
