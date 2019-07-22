package ru.ifmo.onell.util

import java.util.Random

abstract class BinomialScanner {
  def offset(rng: Random): Int
}

object BinomialScanner {
  private[this] object ProbabilityOneScanner extends BinomialScanner {
    override def offset(rng: Random): Int = 1
  }

  private[this] class NormalScanner(probability: Double) extends BinomialScanner {
    private[this] val log1p = math.log1p(-probability)
    override def offset(rng: Random): Int = 1 + (math.log(rng.nextDouble()) / log1p).toInt
  }

  def apply(probability: Double): BinomialScanner = {
    if (probability == 1) ProbabilityOneScanner else new NormalScanner(probability)
  }
}
