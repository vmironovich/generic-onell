package ru.ifmo.onell.distribution

import java.util.Random

import scala.annotation.tailrec

trait IntegerDistribution {
  def sample(rng: Random): Int
}

object IntegerDistribution {
  def constant(value: Int): IntegerDistribution = _ => value

  implicit class Extensions(val underlying: IntegerDistribution) extends AnyVal {
    def max(other: IntegerDistribution): IntegerDistribution = r => math.max(underlying.sample(r), other.sample(r))
    def takeWhen(predicate: Int => Boolean): IntegerDistribution = new TakeWhen(underlying, predicate)
  }

  private class TakeWhen(distribution: IntegerDistribution, predicate: Int => Boolean) extends IntegerDistribution {
    @tailrec
    override final def sample(rng: Random): Int = {
      val current = distribution.sample(rng)
      if (predicate(current)) current else sample(rng)
    }
  }
}
