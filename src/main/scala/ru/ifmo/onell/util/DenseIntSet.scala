package ru.ifmo.onell.util

import java.util.Random

final class DenseIntSet(maxElement: Int) extends OrderedSet[Int] {
  private[this] val contained: Array[Boolean] = Array.ofDim(maxElement)
  private[this] val elements: Array[Int] = Array.ofDim(maxElement)
  private[this] var mySize = 0

  override def size: Int = mySize
  override def apply(index: Int): Int = elements(index)

  override def clear(): Unit = {
    var i = 0
    val iMax = mySize
    val cntd = contained
    val elms = elements
    while (i < iMax) {
      cntd(elms(i)) = false
      i += 1
    }
    mySize = 0
  }

  override def add(element: Int): Unit = {
    val ei = element.toInt
    if (!contained(ei)) {
      contained(ei) = true
      elements(mySize) = ei
      mySize += 1
    }
  }

  override def shuffleOrder(rng: Random): Unit = {
    var i = 1
    while (i < size) {
      val j = rng.nextInt(i + 1)
      if (i != j) {
        val tmp = elements(i)
        elements(i) = elements(j)
        elements(j) = tmp
      }
      i += 1
    }
  }
}
