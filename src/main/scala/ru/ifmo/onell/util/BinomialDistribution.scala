package ru.ifmo.onell.util

import java.util.Random

import ru.ifmo.onell.IntegerDistribution

object BinomialDistribution {
  def naive(n: Int, p: Double): IntegerDistribution = rng => naive(n, p, rng)
  def withScanner(n: Int, p: Double): IntegerDistribution = rng => withScanner(n, p, rng)
  def withOneSampleCounting(n: Int, p: Double): IntegerDistribution = rng => withOneSampleCounting(n, p, rng)

  private def naive(n: Int, p: Double, rng: Random): Int = {
    var result, i = 0
    while (i < n) {
      result += (if (rng.nextDouble() < p) 1 else 0)
      i += 1
    }
    result
  }

  private def withScanner(n: Int, p: Double, rng: Random): Int = {
    val sc = BinomialScanner(p)
    var idx = sc.offset(rng) - 1
    var result = 0
    while (idx < n) {
      result += 1
      idx += sc.offset(rng)
    }
    result
  }

  private def withOneSampleCounting(n: Int, p: Double, rng: Random): Int = {
    var prob = math.pow(1 - p, n)
    var result = 0
    val sample = rng.nextDouble()
    val xFactor = p / (1 - p)
    var total = 1.0
    while (sample < total - prob) {
      total -= prob
      prob *= xFactor * (n - result) / (result + 1)
      result += 1
    }
    result
  }
}
