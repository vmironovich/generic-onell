package ru.ifmo.onell.main

import java.net.{DatagramPacket, DatagramSocket}

import ru.ifmo.onell._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, OneMax, RandomPlanted3SAT}
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}

object IRaceClient extends Main.Module {
  override def name: String = "irace"
  override def shortDescription: String = "Runs computations for irace"
  override def longDescription: Seq[String] = Seq(
    "Executes the given algorithm for the given problem, as configured by the irace parameter tuning program.",
    "The command-line parameters have the following syntax:",
    "  --run-as-server <port1>,<port2>,...: listens on UDP sockets on given ports for packets formatted as below",
    "  --algorithm <algorithm> --problem <problem> <parameters>: directly run the algorithm on the problem,",
    "                                                            which can also be sent through a socket",
    "",
    "The parameters may be related to either the algorithm or the problem.",
    "Common parameters:",
    "  --max-evaluations <long>: the limit on the number of evaluations",
    "",
    "Supported algorithms:",
    "  oll: the (1+(λ,λ)) genetic algorithm. Supported parameters:",
    "    --lambda-tuning  linear|log|fixed: the lambda tuning strategy to use",
    "    --lambda         <double>: the value for lambda to use when --lambda-tuning is 'fixed'",
    "    --tuning-success <double>: the multiple to apply to lambda if fitness is increased",
    "    --tuning-failure <double>: the multiple to apply to lambda if fitness is not increased",
    "    --mutation-strength-alg   standard|shift|resampling: the algorithm to sample mutation strength",
    "    --crossover-strength-alg  standard|shift|resampling: the algorithm to sample crossover strength",
    "    --crossover-strength-base lambda|distance: the denominator for the probability to sample crossover strength",
    "    --good-mutant-strategy    ignore|skip-crossover|do-not-count|do-not-sample: the strategy to treat",
    "                              the mutants which are good enough to compete with parents",
    "    --mutation-q   <double>: the additional multiple to the standard mutation probability",
    "    --crossover-q  <double>: the additional multiple to the standard crossover probability",
    "    --crossover-popsize-q <double>: the additional multiple to the standard crossover population size",
    "    --popsize-rounding:   round-up|round-down|probabilistic: the strategy to compute integer population sizes",
    "",
    "Supported problems:",
    "  OneMax: the famous OneMax function. Supported parameters:",
    "    --n <int>: the problem size",
    "  Linear: the randomly generated linear pseudo-Boolean function. Supported parameters:",
    "    --n          <int>: the problem size",
    "    --seed       <long>: the random seed to use",
    "    --max-weight <double>: the maximum weight to sample (the minimum is always 1.0)",
    "  MaxSat: the satisfiable randomly generated maximum 3CNF-SAT problem. Supported parameters:",
    "    --n         <int>: the number of Boolean variables",
    "    --clauses   <int>: the number of clauses",
    "    --seed     <long>: the random seed to use",
    "    --generator easy|hard: the clause generator to use",
  )

  override def moduleMain(args: Array[String]): Unit = {
    if (args.contains("--run-as-server")) {
      val ports = args.getOption("--run-as-server").split(',').map(_.toInt)
      for (port <- ports) {
        val thread = new Thread(() => runServer(port))
        thread.setName(s"worker-$port")
        println(s"Starting thread ${thread.getName} to listen on port $port")
        thread.start()
      }
    } else {
      println(runMany(args))
    }
  }

  private def runServer(port: Int): Unit = {
    val socket = new DatagramSocket(port)
    val array = new Array[Byte](10240)
    val packet = new DatagramPacket(array, array.length)
    try {
      receiveCommandsUntilTheEnd(socket, array, packet)
    } finally {
      socket.close()
    }
  }

  @scala.annotation.tailrec
  private def receiveCommandsUntilTheEnd(socket: DatagramSocket, theArray: Array[Byte], packet: DatagramPacket): Unit = {
    packet.setData(theArray)
    socket.setSendBufferSize(10240)
    socket.setReceiveBufferSize(10240)
    socket.receive(packet)
    val text = new String(packet.getData, packet.getOffset, packet.getLength)
    val result = runMany(text.split(" "))
    val resultBytes = result.getBytes
    packet.setData(resultBytes)
    socket.send(packet)
    receiveCommandsUntilTheEnd(socket, theArray, packet)
  }

  private def runMany(args: Array[String]): String = try {
    val nRuns = args.getOption("--average-over").toInt
    var sum = 0.0
    var idx = 0
    while (idx < nRuns && !sum.isInfinite) {
      sum += runOne(args)
      idx += 1
    }
    (sum / nRuns).toString
  } catch {
    case e: Throwable => e.toString
  }

  private def runOne(command: Array[String]): Double = {
    val optimizer = parseOptimizer(command.getOption("--algorithm"), command)
    val maxEvaluations = command.getOption("--max-evaluations").toLong
    command.getOption("--problem") match {
      case "OneMax" => runUntilOptimum(optimizer, maxEvaluations,
                                       new OneMax(command.getOption("--n").toInt))
      case "Linear" => runUntilOptimum(optimizer, maxEvaluations,
                                       new LinearRandomDoubleWeights(command.getOption("--n").toInt,
                                                                     command.getOption("--max-weight").toDouble,
                                                                     command.getOption("--seed").toLong))
      case "MaxSat" => runUntilOptimum(optimizer, maxEvaluations,
                                       new RandomPlanted3SAT(command.getOption("--n").toInt,
                                                             command.getOption("--clauses").toInt,
                                                             command.getOption("--generator") match {
                                                               case "easy" => RandomPlanted3SAT.EasyGenerator
                                                               case "hard" => RandomPlanted3SAT.HardGenerator
                                                             },
                                                             command.getOption("--seed").toLong))
      case _ => throw new IllegalArgumentException(s"Unknown problem name '${command(1)}'. Supported names: OneMax, Linear, MaxSat'")
    }
  }

  private def parseOptimizer(optimizerName: String, args: Array[String]): Optimizer = {
    optimizerName match {
      case "oll" => new OnePlusLambdaLambdaGA(
        lambdaTuning = args.getOption("--lambda-tuning") match {
          case "linear" => oneFifthLambda(
            onSuccess = args.getOption("--tuning-success").toDouble,
            onFailure = args.getOption("--tuning-failure").toDouble,
            threshold = size => size
          )
          case "log" => oneFifthLambda(
            onSuccess = args.getOption("--tuning-success").toDouble,
            onFailure = args.getOption("--tuning-failure").toDouble,
            threshold = size => 2 * math.log(size + 1)
          )
          case "fixed" => fixedLambda(args.getOption("--lambda").toDouble)
        },
        mutationStrength = args.getOption("--mutation-strength-alg") match {
          case "standard"   => MutationStrength.Standard
          case "shift"      => MutationStrength.Shift
          case "resampling" => MutationStrength.Resampling
        },
        crossoverStrength = args.getOption("--crossover-strength-alg") match {
          case "standard" => args.getOption("--crossover-strength-base") match {
            case "lambda"   => CrossoverStrength.StandardL
            case "distance" => CrossoverStrength.StandardD
          }
          case "shift" => args.getOption("--crossover-strength-base") match {
            case "lambda"   => CrossoverStrength.ShiftL
            case "distance" => CrossoverStrength.ShiftD
          }
          case "resampling" => args.getOption("--crossover-strength-base") match {
            case "lambda"   => CrossoverStrength.ResamplingL
            case "distance" => CrossoverStrength.ResamplingD
          }
        },
        goodMutantStrategy = args.getOption("--good-mutant-strategy") match {
          case "ignore" => GoodMutantStrategy.Ignore
          case "skip-crossover" => GoodMutantStrategy.SkipCrossover
          case "do-not-count" => GoodMutantStrategy.DoNotCountIdentical
          case "do-not-sample" => GoodMutantStrategy.DoNotSampleIdentical
        },
        constantTuning = ConstantTuning(
          mutationProbabilityQuotient = args.getOption("--mutation-q").toDouble,
          crossoverProbabilityQuotient = args.getOption("--crossover-q").toDouble,
          crossoverPopulationSizeQuotient = args.getOption("--crossover-popsize-q").toDouble
        ),
        populationRounding = args.getOption("--popsize-rounding") match {
          case "round-up" => roundUpPopulationSize
          case "round-down" => roundDownPopulationSize
          case "probabilistic" => probabilisticPopulationSize
        }
      )
      case _ => throw new IllegalArgumentException(s"Unknown optimizer name '$optimizerName'. Supported names: oll")
    }
  }

  private def runUntilOptimum[I, @specialized(fsp) F, @specialized(csp) C]
                             (optimizer: Optimizer, maxEvaluations: Long, problem: Fitness[I, F, C])
                             (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Double = {
    try {
      optimizer.optimize(problem, new SimpleTerminationLogger[F](maxEvaluations)).toDouble / problem.problemSize
    } catch {
      case EvaluationExceededException => Double.PositiveInfinity
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

  private class SimpleTerminationLogger[@specialized(fsp) F](maxEvaluations: Long) extends IterationLogger[F] {
    override def logIteration(evaluations: Long, fitness: F): Unit =
      if (evaluations > maxEvaluations)
        throw EvaluationExceededException
  }

  private object EvaluationExceededException extends RuntimeException
}
