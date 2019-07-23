package ru.ifmo.onell.problem

import org.scalatest.{FlatSpec, Matchers}

class OneMaxPermTests extends FlatSpec with Matchers {
  "OneMaxPerm.unpack" should "work correctly" in {
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
}
