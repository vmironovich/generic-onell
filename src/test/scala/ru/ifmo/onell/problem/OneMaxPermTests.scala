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
        val (j0, j1) = OneMaxPerm.unpack(change)
        j0 shouldBe small
        j1 shouldBe big
        small += 1
        change += 1
      }
      big += 1
    }
  }

  it should "work correctly for large inputs" in {
    val (smaller, bigger) = OneMaxPerm.unpack(1457569539L)
    bigger shouldBe 53992
    smaller shouldBe 28503
  }
}
