package ru.ifmo.onell.util.lriw

import java.math.BigInteger

trait MutationCombinator {
  def compute(signedWeightCount: Array[Int], nBitsToFlip: Int): Array[Array[BigInteger]]
}
