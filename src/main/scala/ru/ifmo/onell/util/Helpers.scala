package ru.ifmo.onell.util

object Helpers {
  def flipEachBit(ind: Array[Boolean], indices: OrderedSet[Int]): Unit = {
    var i = indices.size - 1
    while (i >= 0) {
      ind(indices(i)) ^= true
      i -= 1
    }
  }
}
