package ru.ifmo.onell.delta

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.HasDeltaOperations
import ru.ifmo.onell.util.{OrderedSet, SparseLongSet}

object LongSetOps extends HasDeltaOperations[Long] {
  override def createStorage(nChanges: Long): OrderedSet[Long] = new SparseLongSet()

  override def initializeDelta(delta: OrderedSet[Long], nChanges: Long, size: Int, rng: Random): Unit = {
    delta.clear()
    while (delta.size < size) {
      delta.add(rng.nextLong(nChanges))
    }
  }

  override def initializeDeltaFromExisting(delta: OrderedSet[Long], source: OrderedSet[Long], size: Int, rng: Random): Unit = {
    delta.clear()
    val sourceSize = source.size
    while (delta.size < size) {
      // TODO: this can be much slower than intended if size is almost sourceSize
      delta.add(source(rng.nextInt(sourceSize)))
    }
  }
}
