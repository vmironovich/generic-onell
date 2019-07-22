package ru.ifmo.onell.util

final class IntArraySet(maxElement: Int) {
  private[this] val contained: Array[Boolean] = Array.ofDim(maxElement)
  private[this] val elements: Array[Int] = Array.ofDim(maxElement)
  private[this] var mySize = 0

  def size: Int = mySize
  def test(element: Int): Boolean = contained(element)
  def apply(index: Int): Int = elements(index)

  def clear(): Unit = {
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

  @inline
  def foreach(fun: Int => Unit): Unit = {
    var i = 0
    val iMax = mySize
    val elms = elements
    while (i < iMax) {
      fun(elms(i))
      i += 1
    }
  }

  @inline
  def count(predicate: Int => Boolean): Int = {
    var i = 0
    var cnt = 0
    val iMax = mySize
    val elms = elements
    while (i < iMax) {
      if (predicate(elms(i))) {
        cnt += 1
      }
      i += 1
    }
    cnt
  }

  def += (element: Int): Unit = {
    if (element < 0 || element >= contained.length) {
      throw new IndexOutOfBoundsException
    }
    if (!contained(element)) {
      contained(element) = true
      elements(mySize) = element
      mySize += 1
    }
  }
}
