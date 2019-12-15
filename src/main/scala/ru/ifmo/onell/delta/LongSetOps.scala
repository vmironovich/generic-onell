package ru.ifmo.onell.delta

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.HasDeltaOperations
import ru.ifmo.onell.util.{BinomialScanner, HierarchicIntSet, OrderedSet}

object LongSetOps extends HasDeltaOperations[Long, Long] {
  override def createStorage(nChanges: Long): OrderedSet[Long] = {
    new HierarchicIntSet(nChanges)
  }

  override def initializeDeltaWithDefaultSize(delta: OrderedSet[Long], nChanges: Long, expectedSize: Double, rng: Random): Int = {
    delta.clear()

    val scanner = BinomialScanner(expectedSize / nChanges)
    var index = scanner.offset(rng) - 1L
    while (index < nChanges) {
      delta.add(index)
      index += scanner.offset(rng)
    }

    delta.shuffleOrder(rng)
    delta.size
  }

  override def initializeDeltaWithGivenSize(delta: OrderedSet[Long], nChanges: Long, size: Int, rng: Random): Unit = {
    delta.clear()
    while (delta.size < size) {
      delta.add(rng.nextLong(nChanges))
    }
  }

  override def initializeDeltaFromExisting(delta: OrderedSet[Long], source: OrderedSet[Long], expectedSize: Double, rng: Random): Int = {
    delta.clear()

    val sourceSize = source.size
    val scanner = BinomialScanner(expectedSize / sourceSize)
    var index = scanner.offset(rng) - 1L
    while (index < sourceSize) {
      delta.add(source(index.toInt))
      index += scanner.offset(rng)
    }

    delta.size
  }
}
