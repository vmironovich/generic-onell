package ru.ifmo.onell.util

import java.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.ifmo.onell.IntegerDistribution

class BinomialDistributionTests extends AnyFlatSpec with Matchers {
  @scala.annotation.tailrec
  private def populate(dist: IntegerDistribution, trials: Int, rng: Random, target: Array[Int]): Unit = {
    if (trials > 0) {
      target(dist.sample(rng)) += 1
      populate(dist, trials - 1, rng, target)
    }
  }

  @scala.annotation.tailrec
  private def compareArrays(a: Array[Int], b: Array[Int], idx: Int, delta: Int): Unit = {
    if (idx < a.length) {
      assert(math.abs(a(idx) - b(idx)) <= delta)
      compareArrays(a, b, idx + 1, delta)
    }
  }

  private def compare(n: Int, p: Double, trials: Int): Unit = {
    val rng = new Random(n ^ p.hashCode() ^ (trials * 44438231L))
    val naive, withScanner, withOneCall = new Array[Int](n + 1)
    populate(BinomialDistribution.naive(n, p), trials, rng, naive)
    populate(BinomialDistribution.withScanner(n, p), trials, rng, withScanner)
    populate(BinomialDistribution.withOneSampleCounting(n, p), trials, rng, withOneCall)
    compareArrays(naive, withScanner, 0, math.sqrt(trials).toInt)
    compareArrays(naive, withOneCall, 0, math.sqrt(trials).toInt)
  }

  "distributions" should "be nearly equal for n=100, p=0.01" in compare(100, 0.01, 5000)
  they should "be nearly equal for n=100, p=0.03" in compare(100, 0.03, 5000)
  they should "be nearly equal for n=100, p=0.1" in compare(100, 0.1, 10000)
  they should "be nearly equal for n=1000, p=0.001" in compare(1000, 0.001, 30000)
  they should "be nearly equal for n=1000, p=0.02" in compare(1000, 0.02, 30000)
  they should "be nearly equal for n=1000, p=0.1" in compare(1000, 0.1, 30000)
}
