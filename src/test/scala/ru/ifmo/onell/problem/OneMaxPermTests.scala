package ru.ifmo.onell.problem

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OneMaxPermTests extends AnyFlatSpec with Matchers {
  "OneMaxPerm.unpack" should "work correctly for small inputs" in {
    var change = 0
    var big = 1
    while (big < 1000) {
      var small = 0
      while (small < big) {
        val j0j1 = OneMaxPerm.unpack(change)
        val j0 = j0j1.toInt
        val j1 = (j0j1 >>> 32).toInt

        j0 shouldBe small
        j1 shouldBe big
        small += 1
        change += 1
      }
      big += 1
    }
  }

  it should "work correctly for large inputs" in {
    val pair = OneMaxPerm.unpack(1457569539L)
    val smaller = pair.toInt
    val bigger = (pair >>> 32).toInt
    bigger shouldBe 53992
    smaller shouldBe 28503
  }
}
