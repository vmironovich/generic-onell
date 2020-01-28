package ru.ifmo.onell.util.lriw
import java.math.BigInteger

import ru.ifmo.onell.util.Helpers

object BruteForceMutationCombinator2 extends MutationCombinator {
  override def compute(signedWeightCount: Array[Int], nBitsToFlip: Int): Array[Array[BigInteger]] = {
//    require(signedWeightCount.length % 2 == 0)
//    require(signedWeightCount.forall(_ >= 0))
//    require(nBitsToFlip <= signedWeightCount.sum)

    val maxWeight = signedWeightCount.length / 2
    val combinations = Array.fill(maxWeight * nBitsToFlip * 2 + 1, 2)(BigInteger.ZERO)
    new RecursionPacker(combinations, signedWeightCount, nBitsToFlip)
      .go(0, nBitsToFlip, maxWeight * nBitsToFlip, isBad = true, BigInteger.ONE)
    combinations
  }

  private class RecursionPacker(combinations: Array[Array[BigInteger]], weightCount: Array[Int], nBitsToFlip: Int) {
    def go(weightsSeen: Int, remainingBits: Int, fitnessIndex: Int, isBad: Boolean, curr: BigInteger): Unit = {
      val wc = weightCount(weightsSeen)
      val maxWeight = weightCount.length / 2
      val currWeight = weightsSeen - maxWeight + (if (weightsSeen < maxWeight) 0 else 1)

      if (weightsSeen + 1 == weightCount.length) {
        assert(maxWeight == currWeight)
        if (remainingBits <= wc) {
          val f = combinations(fitnessIndex - remainingBits * currWeight)
          val i = if (isBad) 0 else 1
          f(i) = f(i).add(curr.multiply(Helpers.choose(wc, remainingBits)))
        }
      } else {
        var i = 0
        var choices = BigInteger.ONE
        while (i <= wc && i <= remainingBits) {
          val isBadNew = isBad && (i == 0 || currWeight > 0)
          go(weightsSeen + 1, remainingBits - i, fitnessIndex - i * currWeight, isBadNew, curr.multiply(choices))
          choices = choices.multiply(BigInteger.valueOf(wc - i))
          i += 1
          choices = choices.divide(BigInteger.valueOf(i))
        }
      }
    }
  }
}
