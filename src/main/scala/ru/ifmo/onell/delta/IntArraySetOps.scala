package ru.ifmo.onell.delta

import java.util.Random

import ru.ifmo.onell.HasDeltaOperations
import ru.ifmo.onell.util.{BinomialScanner, IntArraySet}

object IntArraySetOps extends HasDeltaOperations[IntArraySet] {
  override def createStorage(problemSize: Int): IntArraySet = new IntArraySet(problemSize)

  override def initializeDeltaWithDefaultSize(delta: IntArraySet, problemSize: Int, expectedSize: Double, rng: Random): Int = {
    delta.clear()

    val scanner = BinomialScanner(expectedSize / problemSize)
    var index = scanner.offset(rng) - 1
    while (index < problemSize) {
      delta += index
      index += scanner.offset(rng)
    }

    delta.size
  }

  override def initializeDeltaWithGivenSize(delta: IntArraySet, problemSize: Int, size: Int, rng: Random): Unit = {
    delta.clear()
    while (delta.size < size) {
      delta += rng.nextInt(problemSize)
    }
  }

  override def initializeDeltaFromExisting(delta: IntArraySet, source: IntArraySet, expectedSize: Double, rng: Random): Int = {
    delta.clear()

    val sourceSize = source.size
    val scanner = BinomialScanner(expectedSize / sourceSize)
    var index = scanner.offset(rng) - 1
    while (index < sourceSize) {
      delta += source(index)
      index += scanner.offset(rng)
    }

    delta.size
  }
}
