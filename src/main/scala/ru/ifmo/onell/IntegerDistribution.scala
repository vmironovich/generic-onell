package ru.ifmo.onell

import java.util.Random

import scala.annotation.tailrec

trait IntegerDistribution {
  def sample(rng: Random): Int
}

object IntegerDistribution {
  def constant(value: Int): IntegerDistribution = _ => value

  implicit class Extensions(val underlying: IntegerDistribution) extends AnyVal {
    def max(other: IntegerDistribution): IntegerDistribution = r => math.max(underlying.sample(r), other.sample(r))
    def skipUntil(predicate: Int => Boolean): IntegerDistribution = new SkipUntil(underlying, predicate)
  }

  private class SkipUntil(distribution: IntegerDistribution, predicate: Int => Boolean) extends IntegerDistribution {
    @tailrec
    override final def sample(rng: Random): Int = {
      val current = distribution.sample(rng)
      if (predicate(current)) current else sample(rng)
    }
  }
}
