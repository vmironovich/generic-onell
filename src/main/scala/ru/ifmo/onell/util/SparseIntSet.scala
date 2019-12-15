package ru.ifmo.onell.util

import java.util.Random

import scala.collection.mutable

class SparseIntSet extends OrderedSet[Long] {
  private[this] var elements: Array[Long] = new Array[Long](10)
  private[this] var nElements = 0
  private[this] val elementSet = new mutable.HashSet[Long]()

  override def size: Int = nElements

  override def apply(index: Int): Long = elements(index)

  override def clear(): Unit = {
    nElements = 0
    elementSet.clear()
  }

  override def add(element: Long): Unit = {
    if (!elementSet.contains(element)) {
      elementSet += element
      if (nElements == elements.size) {
        elements = Array.copyOf(elements, 2 * elements.length)
      }
      elements(nElements) = element
      nElements += 1
    }
  }

  override def shuffleOrder(rng: Random): Unit = {
    var i = 0
    while (i < nElements) {
      val j = rng.nextInt(i + 1)
      if (j != i) {
        val tmp = elements(i)
        elements(i) = elements(j)
        elements(j) = tmp
      }
      i += 1
    }
  }
}
