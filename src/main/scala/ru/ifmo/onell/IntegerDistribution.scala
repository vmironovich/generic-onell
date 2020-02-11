package ru.ifmo.onell

import java.util.Random

trait IntegerDistribution {
  def sample(rng: Random): Int
}

object IntegerDistribution {
  def constant(value: Int): IntegerDistribution = _ => value
}
