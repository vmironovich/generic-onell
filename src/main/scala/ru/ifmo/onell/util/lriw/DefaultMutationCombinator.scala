package ru.ifmo.onell.util.lriw

import java.math.BigInteger

object DefaultMutationCombinator extends MutationCombinator {
  private[this] def forCombinationsL(sum: Int, len: Int, bounds: Array[Int])(fun: (Array[Int], BigInteger) => Unit): Unit = {
    combinationsL2(sum, 0, bounds, new Array(len), BigInteger.ONE, fun)
  }

  private[this] def combinationsL2(sum: Int, idx: Int, bounds: Array[Int], template: Array[Int], nComb: BigInteger,
                                   fun: (Array[Int], BigInteger) => Unit): Unit = {
    if (idx + 1 == template.length) {
      if (sum <= bounds(idx)) {
        template(idx) = sum
        fun(template, nComb)
      }
    } else {
      val bound = bounds(idx)
      var curr = 0
      var choose = BigInteger.ONE
      while (curr <= bound && curr <= sum) {
        template(idx) = curr
        combinationsL2(sum - curr, idx + 1, bounds, template, nComb.multiply(choose), fun)
        choose = choose.multiply(BigInteger.valueOf(bound - curr))
        curr += 1
        choose = choose.divide(BigInteger.valueOf(curr))
      }
    }
  }

  private[this] def applyTheoreticalDelta(ls: Array[Int]): Int = { // will overflow for large N and lambda.
    val maxWeight = ls.length / 2
    var newFitness = 0
    var i = 0
    while (i < maxWeight) {
      newFitness += ls(i) * (maxWeight - i)
      i += 1
    }
    while (i < maxWeight * 2) {
      newFitness -= ls(i) * (i - maxWeight + 1)
      i += 1
    }
    newFitness
  }

  private[this] def isBadMutation(ls: Array[Int]): Boolean = {
    val maxWeight = ls.length / 2
    var goodGens = 0
    var i = 0
    while (i < maxWeight) {
      goodGens += ls(i)
      i += 1
    }
    goodGens == 0
  }

  override def compute(signedWeightCount: Array[Int], nBitsToFlip: Int): Array[Array[BigInteger]] = {
    require(signedWeightCount.length % 2 == 0)
    require(signedWeightCount.forall(_ >= 0))
    require(nBitsToFlip <= signedWeightCount.sum)

    val maxWeight = signedWeightCount.length / 2
    val combinations = Array.fill(maxWeight * nBitsToFlip * 2 + 1, 2)(BigInteger.ZERO)

    forCombinationsL(nBitsToFlip, signedWeightCount.length, signedWeightCount) { (ls, prob) =>
      val newFitness = applyTheoreticalDelta(ls) + maxWeight * nBitsToFlip
      val mutIndex = if (isBadMutation(ls)) 0 else 1
      combinations(newFitness)(mutIndex) = combinations(newFitness)(mutIndex).add(prob)
    }

    combinations
  }
}
