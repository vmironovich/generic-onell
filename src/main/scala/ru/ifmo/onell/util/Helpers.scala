package ru.ifmo.onell.util

object Helpers {
  def flipEachBit(ind: Array[Boolean], indices: IntSet): Unit = {
    var i = indices.size - 1
    while (i >= 0) {
      ind(indices(i).toInt) ^= true
      i -= 1
    }
  }
}
