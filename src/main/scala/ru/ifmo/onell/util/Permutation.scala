package ru.ifmo.onell.util

final class Permutation private (data: Array[Int]) {
  def apply(index: Int): Int = data(index)
  def size: Int = data.length

  def swap(a: Int, b: Int): Unit = {
    val tmp = data(a)
    data(a) = data(b)
    data(b) = tmp
  }
}

object Permutation {
  def identity(size: Int): Permutation = new Permutation(Array.tabulate(size)(scala.Predef.identity))
}
