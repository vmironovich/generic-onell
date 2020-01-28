package ru.ifmo.onell.util.lriw

import java.math.BigInteger
import java.util.Random

object PerfTest {
  def assertEquals(res1: Array[Array[BigInteger]], res2: Array[Array[BigInteger]]): Unit = {
    assert(res1.length == res2.length)
    for (i <- res1.indices) {
      assert(res1(i).length == res2(i).length)
      for (j <- res1(i).indices) {
        assert(res1(i)(j) == res2(i)(j))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val rng = new Random()

    for (weight <- 2 to 5) {
      for (choose <- 2 to 20 by 2) {
        for (_ <- 0 to 10) {
          val ns = Array.fill(2 * weight)(rng.nextInt(1000))
          if (choose <= ns.sum) {
            val t0 = System.nanoTime()
            val res1 = BruteForceMutationCombinator.compute(ns, choose)
            val t1 = System.nanoTime()
            val res2 = BruteForceMutationCombinator2.compute(ns, choose)
            val t2 = System.nanoTime()
            val res3 = DynamicProgrammingMutationCombinator.compute(ns, choose)
            val t3 = System.nanoTime()

            assertEquals(res1, res2)
            assertEquals(res1, res3)

            println(s"W=$weight, L=$choose, N=${ns.sum}: V1: ${(t1 - t0) * 1e-9} s, V2: ${(t2 - t1) * 1e-9} s, V3: ${(t3 - t2) * 1e-9} s")
          }
        }
      }
    }
  }
}
