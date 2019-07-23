package ru.ifmo.onell.util

import java.util.Random

trait IntSet {
  def size: Int
  def test(element: Int): Boolean
  def apply(index: Int): Int
  def clear(): Unit
  def add(element: Int): Unit
  def shuffleOrder(rng: Random): Unit
}
