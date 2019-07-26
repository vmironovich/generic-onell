package ru.ifmo.onell.util

import java.util.Random

trait IntSet {
  def size: Int
  def test(element: Long): Boolean
  def apply(index: Int): Long
  def clear(): Unit
  def add(element: Long): Unit
  def shuffleOrder(rng: Random): Unit
}
