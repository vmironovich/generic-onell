package ru.ifmo.onell.util.lriw

import java.math.BigInteger

object DynamicProgrammingMutationCombinator extends MutationCombinator {
  override def compute(signedWeightCount: Array[Int], nBitsToFlip: Int): Array[Array[BigInteger]] = {
//    require(signedWeightCount.length % 2 == 0)
//    require(signedWeightCount.forall(_ >= 0))
//    require(nBitsToFlip <= signedWeightCount.sum)

    val maxWeight = signedWeightCount.length / 2
    val weights = Array.tabulate(signedWeightCount.length)(i => i - maxWeight + (if (i < maxWeight) 0 else 1))

    val combinations = Array.fill(maxWeight * nBitsToFlip * 2 + 1, 2)(BigInteger.ZERO)

    val dp = Array.fill(nBitsToFlip + 1, 2 * maxWeight * nBitsToFlip + 1)(BigInteger.ZERO)
    dp(0)(maxWeight * nBitsToFlip) = BigInteger.ONE

    var cwi = weights.length
    while (cwi > 0) {
      cwi -= 1
      val weight = weights(cwi)
      val limit = signedWeightCount(cwi)
      var ell = nBitsToFlip + 1
      while (ell > 0) {
        ell -= 1
        var fit = 2 * maxWeight * nBitsToFlip + 1
        while (fit > 0) {
          fit -= 1
          val curr = dp(ell)(fit)
          if (curr.signum() > 0) {
            var lNow = 1
            var choose = BigInteger.valueOf(limit)
            while (lNow <= limit && ell + lNow <= nBitsToFlip) {
              val newFit = fit - lNow * weight
              dp(ell + lNow)(newFit) = dp(ell + lNow)(newFit).add(curr.multiply(choose))
              choose = choose.multiply(BigInteger.valueOf(limit - lNow))
              lNow += 1
              choose = choose.divide(BigInteger.valueOf(lNow))
            }
          }
        }
      }
      if (cwi == maxWeight) {
        // this means we have just completed the table with the last originally-positive weight
        // so dp(nBitsToFlip) is exactly the number of ways to do "bad" flips
        var i = maxWeight * nBitsToFlip * 2 + 1
        while (i > 0) {
          i -= 1
          combinations(i)(0) = dp(nBitsToFlip)(i)
        }
      }
    }

    // we now completed the table, and the total number of ways is in dp(nBitsToFlip)
    var i = maxWeight * nBitsToFlip * 2 + 1
    while (i > 0) {
      i -= 1
      combinations(i)(1) = dp(nBitsToFlip)(i).subtract(combinations(i)(0))
      assert(combinations(i)(1).signum() >= 0)
    }

    combinations
  }
}
