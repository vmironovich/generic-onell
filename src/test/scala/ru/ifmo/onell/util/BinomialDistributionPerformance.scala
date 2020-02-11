package ru.ifmo.onell.util

import java.util.Random

import ru.ifmo.onell.IntegerDistribution

object BinomialDistributionPerformance {
  @scala.annotation.tailrec
  private def go(dist: IntegerDistribution, remains: Int, sum: Double, rng: Random): Double = {
    if (remains == 0) sum else go(dist, remains - 1, sum + dist.sample(rng), rng)
  }

  def main(args: Array[String]): Unit = {
    val kind = args(0)
    val n = args(1).toInt
    val p = args(2).toDouble
    val trials = args(3).toInt

    val dist = kind match {
      case "naive" => BinomialDistribution.naive(n, p)
      case "scanner" => BinomialDistribution.withScanner(n, p)
      case "sample" => BinomialDistribution.withOneSampleCounting(n, p)
    }

    val rng = new Random()
    while (System.in.available() == 0) {
      val t0 = System.nanoTime()
      val checksum = go(dist, trials, 0.0, rng)
      val time = System.nanoTime() - t0
      println(s"${time * 1e-9 / trials} per trial, checksum = $checksum")
    }
  }
}
