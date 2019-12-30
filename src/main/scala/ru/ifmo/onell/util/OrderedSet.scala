package ru.ifmo.onell.util

import java.util.Random

trait OrderedSet[@specialized(Specialization.changeSpecialization) T] {
  def size: Int
  def apply(index: Int): T
  def clear(): Unit
  def add(element: T): Unit
  def shuffleOrder(rng: Random): Unit

  def copyFrom(that: OrderedSet[T]): Unit = {
    clear()
    val thatSize = that.size
    var i = 0
    while (i < thatSize) {
      add(that(i))
      i += 1
    }
  }
}
