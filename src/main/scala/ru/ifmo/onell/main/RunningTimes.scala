package ru.ifmo.onell.main

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.{Arrays => JArrays, Random}
import java.util.concurrent.ThreadLocalRandom

import scala.jdk.CollectionConverters._
import scala.util.Using
import ru.ifmo.onell.{HasIndividualOperations, Main}
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA}
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, LinearRandomIntegerWeights, OneMax, OneMaxPerm, RandomPlanted3SAT, WModel, WModelPerm}
import ru.ifmo.onell.util.par.{Executor, Multiplexer, ParallelExecutor, SequentialExecutor}

import io.circe._, io.circe.parser._, io.circe.generic.semiauto._

import scala.io.Source._


object RunningTimes extends Main.Module {
  override def name: String = "runtime"

  override def shortDescription: String = "Runs experiments on expected running times of algorithms on problems"

  override def longDescription: Seq[String] = Seq(
    "The following commands run experiments for problems on bit strings:",
    "  bits:om         <context>: for OneMax",
    "  bits:om:sqrt    <context>: same but starting at the distance of sqrt(n) from the end",
    "  bits:om:log     <context>: same but starting at the distance of log(n+1) from the end",
    "  bits:om:lin     <context>: same but starting at the distance of d from the end, d is passed with --d option",
    "                             (several values may be passed comma-separated)",
    "  bits:sat        <context>: same for the MAX-SAT problem with logarithmic density",
    "  bits:sat:sqrt   <context>: same but starting at the distance of sqrt(n) from the end",
    "  bits:sat:log    <context>: same but starting at the distance of log(n+1) from the end",
    "  bits:sat:lin    <context>: same but starting at the distance of d from the end, d is passed with --d option",
    "                             (several values may be passed comma-separated)",
    "  bits:om:tuning  <context>: for OneMax with various tuning choices for the (1+(λ,λ)) GA",
    "  bits:l2d:tuning <context>: same for linear functions with random weights from [1;2]",
    "  bits:l5d:tuning <context>: same for linear functions with random weights from [1;5]",
    "  bits:sat:tuning <context>: same for the MAX-SAT problem with logarithmic density",
    "  bits:om:tuning*  <context> <file1>,<file2>,...: for OneMax with various tuning choices",
    "                                                 for the (1+(λ,λ)) GA with constants tuned by irace",
    "  bits:l2d:tuning* <context> <file1>,<file2>,...: same for linear functions with random weights from [1;2]",
    "  bits:l5d:tuning* <context> <file1>,<file2>,...: same for linear functions with random weights from [1;5]",
    "  bits:sat:tuning* <context> <file1>,<file2>,...: same for the MAX-SAT problem with logarithmic density",
    "  bits:l2d:lambda <context>: experiments for lambda tunings for linear functions with random real-valued weights from [1;2]",
    "  bits:l5d:lambda <context>: same for linear functions with random real-valued weights from [1;5]",
    "  bits:om:lambda  <context>: same for OneMax",
    "  bits:l2i:lambda <context>: same for linear functions with random integer weights from [1;2]",
    "  bits:l5i:lambda <context>: same for linear functions with random integer weights from [1;5]",
    "  bits:lni:lambda <context>: same for linear functions with random integer weights from [1;n]",
    "  bits:sat:lambda <context>: same for the MAX-SAT problem with logarithmic density",
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
    case "bits:om"         => bitsOneMaxSimple(parseContext(args))
    case "bits:om:nn"      => bitsOMStaticNNTuning(parseContext(args), args)
    case "bits:wm"         => bitsWModel(parseContext(args), args)
    case "bits:wm:constants" => bitsWModelConstants(parseContext(args), args)
    case "bits:wmfrac:constants" => bitsWModelFracConstants(parseContext(args), args)
    case "bits:wm:tuned" => bitsWModelTuned(parseContext(args), args)
    case "bits:wm:tuned:json" => bitsWModelTunedJson(args)
    case "bits:om:sqrt"    => bitsOneMaxAlmostOptimal(parseContext(args), n => Seq(math.sqrt(n).toInt))
    case "bits:om:log"     => bitsOneMaxAlmostOptimal(parseContext(args), n => Seq(math.log(n + 1).toInt))
    case "bits:l2d:lambda" => bitsParameterTuningLinearDouble(parseContext(args), 2.0)
    case "bits:l5d:lambda" => bitsParameterTuningLinearDouble(parseContext(args), 5.0)
    case "bits:om:lambda"  => bitsParameterTuningLinearInteger(parseContext(args), _ => 1)
    case "bits:l2i:lambda" => bitsParameterTuningLinearInteger(parseContext(args), _ => 2)
    case "bits:l5i:lambda" => bitsParameterTuningLinearInteger(parseContext(args), _ => 5)
    case "bits:lni:lambda" => bitsParameterTuningLinearInteger(parseContext(args), n => n)
    case "bits:sat:lambda" => bitsParameterTuningMaxSAT(parseContext(args))
    case "bits:sat"        => bitsMaxSATSimple(parseContext(args))
    case "bits:sat:sqrt"   => bitsMaxSATAlmostOptimal(parseContext(args), n => Seq(math.sqrt(n).toInt))
    case "bits:sat:log"    => bitsMaxSATAlmostOptimal(parseContext(args), n => Seq(math.log(n + 1).toInt))
    case "perm:om"         => permOneMaxSimple(parseContext(args))
    case "perm:wm"         => permWModelSimple(parseContext(args), args)
    case "bits:om:tuning"  => bitsOneMaxAllTuningChoices(parseContext(args))
    case "bits:l2d:tuning" => bitsLinearTunings(parseContext(args), 2.0)
    case "bits:l5d:tuning" => bitsLinearTunings(parseContext(args), 5.0)
    case "bits:sat:tuning" => bitsMaxSatTunings(parseContext(args))
    case "bits:om:tuning*" => bitsOneMaxIRacedTuningChoices(parseContext(args), args.getOption("--files"))
    case "bits:l2d:tuning*" => bitsLinearDoubleIRacedTuningChoices(parseContext(args), 2.0, args.getOption("--files"))
    case "bits:l5d:tuning*" => bitsLinearDoubleIRacedTuningChoices(parseContext(args), 5.0, args.getOption("--files"))
    case "bits:sat:tuning*" => bitsMaxSatIRacedTuningChoices(parseContext(args), args.getOption("--files"))
    case "bits:om:lin" =>
      val distances = args.getOption("--d").split(',').toIndexedSeq.map(_.toInt)
      bitsOneMaxAlmostOptimal(parseContext(args), _ => distances)
    case "bits:sat:lin" =>
      val distances = args.getOption("--d").split(',').toIndexedSeq.map(_.toInt)
      bitsMaxSATAlmostOptimal(parseContext(args), _ => distances)
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
    //   "RLS" -> OnePlusOneEA.RLS,
    //   "(1+1) EA" -> OnePlusOneEA.Resampling,
    //   "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'D'),
    "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D'),
    "(1+(λ,λ)) GA, λ=4" -> new OnePlusLambdaLambdaGA(fixedLambda(4), 'R', "RL", 'C', 'U'),
    //   "(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(fixedLambda(8), 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ=10" -> new OnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ=12" -> new OnePlusLambdaLambdaGA(fixedLambda(12), 'R', "RL", 'C', 'D'),
    //   "(1+(λ,λ)) GA, λ=fixed optimal" -> new OnePlusLambdaLambdaGA(fixedLogTowerLambda, 'R', "RL", 'C', 'D'),
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

  private val algoStaticLambdaChoice = {
    for {
      l <- Seq(2,4,6,8,10)
    } yield {
      s""""l":$l""" -> new OnePlusLambdaLambdaGA(fixedLambda(l), 'R', "RL", 'C', 'D')
    }
  }

  private def bitsOMStaticNNTuning(context: Context, args : Array[String]): Unit = {
    context.run { (scheduler, n) =>
      for ((jsonName, algGenerator) <- algoStaticLambdaChoice) {
        scheduler addTask {
          val nc = args.getOption("--nc").toDouble
          val trueN = (nc*n).toInt
          val time = algGenerator.optimize(new OneMax(trueN))
          s"""{"n":$trueN,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsWModel(context: Context, args : Array[String]): Unit = {
    val algorithms = Seq(
      //"RLS" -> OnePlusOneEA.RLS,
      //"(1+1) EA" -> OnePlusOneEA.Resampling,
      //"(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U'),
      //"(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=4" -> new OnePlusLambdaLambdaGA(fixedLambda(4), 'R', "RL", 'C', 'U'),
      //"(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(fixedLambda(8), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=10" -> new OnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=12" -> new OnePlusLambdaLambdaGA(fixedLambda(12), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=fixed optimal" -> new OnePlusLambdaLambdaGA(fixedLogTowerLambda, 'R', "RL", 'C', 'D'),
    )
    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        for (neu <- Range(0,8)) {
        scheduler addTask {
          val dummy = args.getOption("--dummy").toDouble
          val epi = args.getOption("--epi").toInt
          //val neu = args.getOption("--neu").toInt
          val rug = args.getOption("--rug").toInt
          val time = alg.optimize(new WModel(n, dummy, epi, neu, rug))
          s"""{"n":$n,"dummy":$dummy,"epi":$epi,"neu":$neu,"rug":$rug, "algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
        }
      }
    }
  }


  private val constantTuningChoices = {
    for {
      l1 <- Range(2,10,1)
      l2 <- Range(2,10,1)
      c <-  Range(1,10,2)
      k <- Range(1,10,1)
    } yield {
      val jsonNamePart = s""""lambdaOne":"$l1","lambda2":"$l2","crossover":"${c*0.01}","mutation":"$k""""
      val algGenerator = () => new OnePlusLambdaLambdaGA(fixedLambda(l1),'R', "RL", 'C', 'D', new ConstantTuning(k, 0.01* c * l1, l2/l1) )
      jsonNamePart -> algGenerator
    }
  }

  private def bitsWModelTuned(context: Context, args: Array[String]) : Unit = {
    val c = args.getOption("--c").toDouble
    val l1 = args.getOption("--l1").toInt
    val l2 = args.getOption("--l2").toInt
    val k = args.getOption("--k").toInt
    val algorithms = Seq(
      //"RLS" -> OnePlusOneEA.RLS,
      //"(1+1) EA" -> OnePlusOneEA.Resampling,
      //"(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U'),
      //"(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U'),
      //"(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=4" -> new OnePlusLambdaLambdaGA(fixedLambda(4), 'R', "RL", 'C', 'U'),
      //"(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(fixedLambda(8), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=10" -> new OnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=12" -> new OnePlusLambdaLambdaGA(fixedLambda(12), 'R', "RL", 'C', 'D'),
      //"(1+(λ,λ)) GA, λ=fixed optimal" -> new OnePlusLambdaLambdaGA(fixedLogTowerLambda, 'R', "RL", 'C', 'D'),
      //"tuned" -> new OnePlusLambdaLambdaGA(fixedLambda(l1),'R', "RL", 'C', 'D', new ConstantTuning(k, c, l2/l1)),
      "best_in_dataset" -> new OnePlusLambdaLambdaGA(fixedLambda(l1),'R', "RL", 'C', 'D', new ConstantTuning(k, c, l2/l1)),
    )
    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val n = args.getOption("--n").toInt
          val dummy = args.getOption("--dummy").toDouble
          val epi = args.getOption("--epi").toInt
          val neu = args.getOption("--neu").toInt
          val rug = args.getOption("--rug").toInt
          val id = args.getOption("--id").toInt
          val time = alg.optimize(new WModel(n, dummy, epi, neu, rug))
          s"""{"id":$id,"n":$n,"dummy":$dummy,"epi":$epi,"neu":$neu,"rug":$rug, "algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  case class JSONRuntime(n : Double, dummy : Double, epi: Double, neu: Double,
    rug : Double, lambdaOne : Double, lambda2 : Double, crossover : Double, mutation : Double)
  implicit val runtimeDecoder: Decoder[JSONRuntime] = deriveDecoder



  private def bitsWModelTunedJson(args: Array[String]) : Unit = {
    val modelFile = args.getOption("--model_file").toString
    val jsonString = fromFile(modelFile).mkString
    val decoded = decode[List[JSONRuntime]](jsonString)
    val unpacked = decoded match {
      case Right(x) => x
      case Left(x) => List[JSONRuntime]()
    }
    print(unpacked(0))
    val first = args.getOption("--from").toInt
    var last = args.getOption("--to").toInt
    if (last == 0) {
      last = unpacked.length
    }
    for (i <- first to last) {
      val l1 = unpacked(i).lambdaOne.toInt //2
      val l2 = unpacked(i).lambda2.toInt //2
      val k = unpacked(i).mutation.toInt //8
      val c = Math.round(unpacked(i).crossover*100.0)/100.0 //0.01
      val algorithms = Seq(
        "RLS" -> OnePlusOneEA.RLS,
        "(1+1) EA" -> OnePlusOneEA.Resampling,
        "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U'),
        "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U'),
        //"(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D'),
        //"(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D'),
        "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
        //"(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D'),
        //"(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D'),
        "(1+(λ,λ)) GA, λ=4" -> new OnePlusLambdaLambdaGA(fixedLambda(4), 'R', "RL", 'C', 'U'),
        //"(1+(λ,λ)) GA, λ=8" -> new OnePlusLambdaLambdaGA(fixedLambda(8), 'R', "RL", 'C', 'D'),
        //"(1+(λ,λ)) GA, λ=10" -> new OnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D'),
        //"(1+(λ,λ)) GA, λ=12" -> new OnePlusLambdaLambdaGA(fixedLambda(12), 'R', "RL", 'C', 'D'),
        //"(1+(λ,λ)) GA, λ=fixed optimal" -> new OnePlusLambdaLambdaGA(fixedLogTowerLambda, 'R', "RL", 'C', 'D'),
        "tuned" -> new OnePlusLambdaLambdaGA(fixedLambda(l1),'R', "RL", 'C', 'D', new ConstantTuning(k, c*l1, l2/l1)),
        "trueSB" -> new OnePlusLambdaLambdaGA(fixedLambda(2),'R', "RL", 'C', 'D', new ConstantTuning(8, 0.01*2, 1.0)),
      )
      val myContext = customContext(args, "tuned_big/"+s"${i/10000}/".reverse.padTo(3,'0').reverse+s"$i.json".reverse.padTo(11,'0').reverse)
      myContext.run { (scheduler, n) =>
        for ((name, alg) <- algorithms) {
          scheduler addTask {
            val n =  unpacked(i).n.toInt
            val dummy = unpacked(i).dummy
            val epi = unpacked(i).epi.toInt
            val neu = unpacked(i).neu.toInt
            val rug = unpacked(i).rug.toInt
            val time = alg.optimize(new WModel(n, dummy, epi, neu, rug))
            s"""{"id":$i,"n":$n,"dummy":$dummy,"epi":$epi,"neu":$neu,"rug":$rug, "lambdaOne":"$l1", "lambda2":"$l2", "crossover":"$c", "mutation":"$k","algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
          }
        }
      }
    }

  }

  private def bitsWModelConstants(context: Context, args : Array[String]): Unit = {
    context.run { (scheduler, n) =>
      for ((jsonName, algGenerator) <- constantTuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(initWMod(n, args))
          val dummy = args.getOption("--dummy").toDouble
          val epi = args.getOption("--epi").toInt
          val neu = args.getOption("--neu").toInt
          val rug = args.getOption("--rug").toInt
          s"""{"n":$n,"dummy":$dummy,"epi":$epi,"neu":$neu,"rug":$rug,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsWModelFracConstants(context: Context, args : Array[String]): Unit = {
    context.run { (scheduler, n) =>
      for ((jsonName, algGenerator) <- constantTuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(initWModFrac(n, args))
          val dummy = args.getOption("--dummy").toDouble
          val epiF = args.getOption("--epi").toDouble
          val epi = (epiF*n).toInt
          val neuF = args.getOption("--neu").toDouble
          val neu = (neuF*n).toInt
          val rugF = args.getOption("--rug").toDouble
          val rug = (rugF*n).toInt
          s"""{"n":$n,"dummy":$dummy,"epi":$epi,"neu":$neu,"rug":$rug,"epiF":$epiF,"neuF":$neuF,"rugF":$rugF,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsOneMaxAlmostOptimal(context: Context, startValues: Int => Seq[Int]): Unit = {
    val algorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D'),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        for (sv <- startValues(n)) {
          implicit val almostOptimalBitStringOps: HasIndividualOperations[Array[Boolean]] = new StartFromDistance(sv)
          scheduler addTask {
            val time = alg.optimize(new OneMax(n))(indOps = almostOptimalBitStringOps, deltaOps = implicitly)
            s"""{"n":$n,"algorithm":"$name","runtime":$time,"expected initial distance":$sv,"runtime over n":${time.toDouble / n}}"""
          }
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
    val populationSizeRoundings = Seq("round down" -> PopulationSizeRounding.AlwaysDown,
                                      "round up" -> PopulationSizeRounding.AlwaysUp,
                                      "probabilistic" -> PopulationSizeRounding.Probabilistic)
    for {
      (l, lambdaStrategy) <- lambdaStrategies
      (m, mutationStrength) <- mutationStrengths
      (c, crossoverStrength) <- crossoverStrengths
      (g, goodMutantStrategy) <- goodMutantStrategies
      (r, rounding) <- populationSizeRoundings
    } yield {
      val jsonNamePart = s""""lambda":"$l","mutation":"$m","crossover":"$c","good mutant":"$g","rounding":"$r""""
      val algGenerator = () => new OnePlusLambdaLambdaGA(lambdaStrategy, mutationStrength, crossoverStrength,
                                                         goodMutantStrategy, rounding)
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

  private val parameterTuningExperimentAlgorithmSelectionDouble = Seq(
    ("RLS", OnePlusOneEA.RLS),
    ("(1+1) EA", OnePlusOneEA.Standard),
    ("*(1+1) EA", OnePlusOneEA.Shift),
    ("$\\\\lambda=8$", new OnePlusLambdaLambdaGA(fixedLambda(8), 'S', "SL", 'I', 'U')),
    ("$\\\\lambdabound=n$", new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'S', "SL", 'I', 'U')),
    ("*$\\\\lambda=8$", new OnePlusLambdaLambdaGA(fixedLambda(8), 'H', "HD", 'C', 'U')),
    ("*$\\\\lambdabound=n$", new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'H', "HD", 'C', 'U')),
  )

  private val parameterTuningExperimentAlgorithmSelection = Seq(
    ("RLS", OnePlusOneEA.RLS),
    ("(1+1) EA", OnePlusOneEA.Standard),
    ("*(1+1) EA", OnePlusOneEA.Shift),
    ("$\\\\lambda=8$", new OnePlusLambdaLambdaGA(fixedLambda(8), 'S', "SL", 'I', 'P')),
    ("$\\\\lambdabound=n$", new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'S', "SL", 'I', 'P')),
    ("$\\\\lambdabound\\\\sim\\\\ln n$", new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'S', "SL", 'I', 'P')),
    ("*$\\\\lambda=8$", new OnePlusLambdaLambdaGA(fixedLambda(8), 'H', "HD", 'C', 'P')),
    ("*$\\\\lambdabound=n$", new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'H', "HD", 'C', 'P')),
    ("*$\\\\lambdabound\\\\sim\\\\ln n$", new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'H', "HD", 'C', 'P')),
  )

  private def bitsParameterTuningLinearDouble(context: Context, maxWeight: Double): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, alg) <- parameterTuningExperimentAlgorithmSelectionDouble) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new LinearRandomDoubleWeights(n, maxWeight, seeder.nextLong()))
          val wcTime = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$wcTime}"""
        }
      }
    }
  }

  private def bitsParameterTuningLinearInteger(context: Context, maxWeight: Int => Int): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, alg) <- parameterTuningExperimentAlgorithmSelection) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new LinearRandomIntegerWeights(n, maxWeight(n), seeder.nextLong()))
          val wcTime = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$wcTime}"""
        }
      }
    }
  }

  private def bitsParameterTuningMaxSAT(context: Context): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for ((name, alg) <- parameterTuningExperimentAlgorithmSelection) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          val consumed = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$consumed}"""
        }
      }
    }
  }

  private def bitsLinearTunings(context: Context, maxWeight: Double): Unit = {
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

  private def bitsLinearDoubleIRacedTuningChoices(context: Context, maxWeight: Double, fileList: String): Unit = {
    val seeder = new Random(314252354)
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
          val time = algorithm.optimize(new LinearRandomDoubleWeights(n, maxWeight, seeder.nextLong()))
          s"""{"n":$n,"irace":1,$name,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ<=2ln n" -> new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> new OnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'U'),
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

  private def bitsMaxSatIRacedTuningChoices(context: Context, fileList: String): Unit = {
    val seeder = new Random(314252354)
    val allLines = fileList
      .split(',')
      .flatMap(filename => Files
        .readAllLines(Paths.get(filename))
        .asScala
        .filter(_.nonEmpty)
        .toIndexedSeq)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for (line <- allLines) {
        scheduler addTask {
          val args = line.split(" ").filter(_.nonEmpty)
          val name = IRaceClient.parseOptimizerJson("oll", args)
          val algorithm = IRaceClient.parseOptimizer("oll", args)
          val time = algorithm.optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          s"""{"n":$n,"irace":1,$name,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATAlmostOptimal(context: Context, startValues: Int => Seq[Int]): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, OnePlusOneEA.RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.Resampling),
      ("(1+(λ,λ)) GA, λ<=n", 16384, new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.1)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.3)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.5)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.7)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.9)", Int.MaxValue, new OnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'U')),
    )

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, limit, alg) <- algorithms) {
        if (n <= limit) {
          for (sv <- startValues(n)) {
            implicit val almostOptimalBitStringOps: HasIndividualOperations[Array[Boolean]] = new StartFromDistance(sv)
            scheduler addTask {
              val time = alg.optimize(new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt,
                                                            RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
              s"""{"n":$n,"algorithm":"$name","runtime":$time,"initial distance":$sv,"runtime over n":${time.toDouble / n}}"""
            }
          }
        }
      }
    }
  }

  private def permOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, OnePlusOneEA.RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.Resampling),
      ("(1+(λ,λ)) GA, λ=10", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLogLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ<=n", 256, new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')),
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

  private def permWModelSimple(context: Context, args : Array[String]): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, OnePlusOneEA.RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.Resampling),
      ("(1+(λ,λ)) GA, λ=10", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ=4", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLambda(4), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(fixedLogLambda, 'R', "RL", 'C', 'U')),
      //("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')),
      //("(1+(λ,λ)) GA, λ<=n", 256, new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')),
    )

    context.run { (scheduler, n) =>
      for ((name, maxN, alg) <- algorithms) {
        if (n <= maxN) {
          scheduler.addTask {
            val time = alg.optimize(initWModPerm(n, args))
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

  private class StartFromDistance(start: Int) extends HasIndividualOperations[Array[Boolean]] {
    override def createStorage(problemSize: Int): Array[Boolean] = new Array(problemSize)
    override def initializeRandomly(individual: Array[Boolean], rng: ThreadLocalRandom): Unit = {
      var remainingPoints = start
      val len = individual.length
      JArrays.fill(individual, true)
      while (remainingPoints > 0) {
        val idx = rng.nextInt(len)
        if (individual(idx)) {
          individual(idx) = false
          remainingPoints -= 1
        }
      }
    }
  }

  private def parseContext(args: Array[String]): Context = new Context(
    powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
    outName  = args.getOption("--out")
  )

  private def customContext(args: Array[String], outName : String): Context = new Context(
    powers = 1 to 1,
    nRuns = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
    outName = outName
  )

  private def initWMod(n : Int, args : Array[String]) : WModel = new WModel(
    n,
    args.getOption("--dummy").toDouble,
    args.getOption("--epi").toInt,
    args.getOption("--neu").toInt,
    args.getOption("--rug").toInt
  )


  private def initWModPerm(n : Int, args : Array[String]) : WModelPerm = new WModelPerm(
    n,
    args.getOption("--dummy").toDouble,
    args.getOption("--epi").toInt,
    args.getOption("--neu").toInt,
    args.getOption("--rug").toInt
  )

  private def initWModFrac(n : Int, args : Array[String]) : WModel = new WModel(
    n,
    args.getOption("--dummy").toDouble,
    (args.getOption("--epi").toDouble * n).toInt,
    (args.getOption("--neu").toDouble * n).toInt,
    (args.getOption("--rug").toDouble * n).toInt
  )
}
