package ru.ifmo.onell.distribution

import java.util.Random

trait IntegerDistribution {
  import IntegerDistribution._

  def sample(rng: Random): Int

  def max(other: IntegerDistribution): IntegerDistribution = r => math.max(sample(r), other.sample(r))
  def takeWhen(predicate: Int => Boolean): IntegerDistribution = new TakeWhen(this, predicate)
}

object IntegerDistribution {
  def constant(value: Int): IntegerDistribution = _ => value

  class TakeWhen(distribution: IntegerDistribution, predicate: Int => Boolean) extends IntegerDistribution {
    @scala.annotation.tailrec
    override final def sample(rng: Random): Int = {
      val current = distribution.sample(rng)
      if (predicate(current)) current else sample(rng)
    }
  }
}
