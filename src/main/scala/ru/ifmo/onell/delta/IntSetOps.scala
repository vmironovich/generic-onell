package ru.ifmo.onell.delta

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.HasDeltaOperations
import ru.ifmo.onell.util.{BinomialScanner, DenseIntSet, OrderedSet}

object IntSetOps extends HasDeltaOperations[Int] {
  override def createStorage(nChanges: Int): OrderedSet[Int] = new DenseIntSet(nChanges)

  override def initializeDeltaWithDefaultSize(delta: OrderedSet[Int], nChanges: Int, expectedSize: Double, rng: Random): Int = {
    delta.clear()

    val scanner = BinomialScanner(expectedSize / nChanges)
    var index = scanner.offset(rng) - 1L
    while (index < nChanges) {
      delta.add(index.toInt)
      index += scanner.offset(rng)
    }

    delta.shuffleOrder(rng)
    delta.size
  }

  override def initializeDeltaWithGivenSize(delta: OrderedSet[Int], nChanges: Int, size: Int, rng: Random): Unit = {
    delta.clear()
    while (delta.size < size) {
      delta.add(rng.nextInt(nChanges))
    }
  }

  override def initializeDeltaFromExisting(delta: OrderedSet[Int], source: OrderedSet[Int], expectedSize: Double, rng: Random): Int = {
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
