package ru.ifmo.onell.util

import java.lang.{Long => JLong}
import java.util.{Arrays => JArrays, Random}

class HierarchicIntSet(maxElement: Long) extends OrderedSet[Long] {
  private[this] val nonZeroBits = 64 - JLong.numberOfLeadingZeros(maxElement - 1)
  private[this] val lowerHalfBits = nonZeroBits / 2
  private[this] val lowerHalfMask = (1 << lowerHalfBits) - 1

  private[this] var elementsInOrder: Array[Long] = new Array(10)
  private[this] var elementCount = 0
  private[this] var arrayPool: Array[Array[Boolean]] = new Array(10)
  private[this] var arrayPoolSize = 0

  private[this] val containment = new Array[Array[Boolean]](1 << (nonZeroBits - lowerHalfBits))
  private[this] val containmentSize = new Array[Int](1 << (nonZeroBits - lowerHalfBits))

  private[this] def hiIndex(index: Long): Int = (index >>> lowerHalfBits).toInt
  private[this] def loIndex(index: Long): Int = (index & lowerHalfMask).toInt

  override def size: Int = elementCount
  override def apply(index: Int): Long = elementsInOrder(index)

  override def clear(): Unit = {
    while (elementCount > 0) {
      elementCount -= 1
      val element = elementsInOrder(elementCount)
      val hi = hiIndex(element)
      val lo = loIndex(element)
      containment(hi)(lo) = false
      containmentSize(hi) -= 1
      if (containmentSize(hi) == 0) {
        if (arrayPool.length == arrayPoolSize) {
          arrayPool = JArrays.copyOf(arrayPool, arrayPoolSize * 2)
        }
        arrayPool(arrayPoolSize) = containment(hi)
        containment(hi) = null
        arrayPoolSize += 1
      }
    }
  }

  override def add(element: Long): Unit = {
    val hi = hiIndex(element)
    val lo = loIndex(element)
    val subArray = { // If there is none so far, we allocate a new one
      val attempt0 = containment(hi)
      if (attempt0 == null) {
        val attempt1 = if (arrayPoolSize == 0) {
          new Array[Boolean](1 + lowerHalfMask)
        } else {
          arrayPoolSize -= 1
          val res = arrayPool(arrayPoolSize)
          arrayPool(arrayPoolSize) = null
          res
        }
        containment(hi) = attempt1
        attempt1
      } else attempt0
    }
    if (!subArray(lo)) {
      subArray(lo) = true
      if (elementCount == elementsInOrder.length) {
        elementsInOrder = JArrays.copyOf(elementsInOrder, 2 * elementCount)
      }
      elementsInOrder(elementCount) = element
      elementCount += 1
      containmentSize(hi) += 1
    }
  }

  override def shuffleOrder(rng: Random): Unit = {
    var i = 1
    while (i < size) {
      val j = rng.nextInt(i + 1)
      if (i != j) {
        val tmp = elementsInOrder(i)
        elementsInOrder(i) = elementsInOrder(j)
        elementsInOrder(j) = tmp
      }
      i += 1
    }
  }
}
