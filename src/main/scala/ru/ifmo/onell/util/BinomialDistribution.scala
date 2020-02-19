package ru.ifmo.onell.util

import java.util.Random

import ru.ifmo.onell.IntegerDistribution

object BinomialDistribution {
  def apply(n: Long, p: Double): IntegerDistribution = rng => withScanner(n, p, rng)

  private def withScanner(n: Long, p: Double, rng: Random): Int = {
    val sc = BinomialScanner(p)
    var idx = sc.offset(rng) - 1
    var result = 0
    while (idx < n) {
      result += 1
      idx += sc.offset(rng)
    }
    result
  }
}
