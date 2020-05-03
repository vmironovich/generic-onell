package ru.ifmo.onell.delta

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.HasDeltaOperations
import ru.ifmo.onell.util.{DenseIntSet, OrderedSet}

object IntSetOps extends HasDeltaOperations[Int] {
  override def createStorage(nChanges: Int): OrderedSet[Int] = new DenseIntSet(nChanges)

  override def initializeDelta(delta: OrderedSet[Int], nChanges: Int, size: Int, rng: Random): Unit = {
    delta.clear()
    while (delta.size < size) {
      delta.add(rng.nextInt(nChanges))
    }
  }

  override def initializeDeltaFromExisting(delta: OrderedSet[Int], source: OrderedSet[Int], size: Int, rng: Random): Unit = {
    delta.clear()
    val sourceSize = source.size
    while (delta.size < size) {
      // TODO: this can be much slower than intended if size is almost sourceSize
      delta.add(source(rng.nextInt(sourceSize)))
    }
  }
}
