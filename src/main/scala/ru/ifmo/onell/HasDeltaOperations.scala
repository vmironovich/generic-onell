package ru.ifmo.onell

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.delta.{IntSetOps, LongSetOps}
import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp}

/**
  * This trait encapsulates the knowledge about manipulations with delta representations.
  *
  * @tparam ChangeType the type of a single element of the delta.
  * @tparam ChangeSizeType the integer type of the integer
  */
trait HasDeltaOperations[@specialized(csp) ChangeType, @specialized(csp) ChangeSizeType] {
  /**
    * Creates a new delta representation, given the number of possible changes.
    * @param nChanges the number of possible changes.
    * @return the newly created delta representation.
    */
  def createStorage(nChanges: ChangeSizeType): OrderedSet[ChangeType]

  /**
    * Initializes the given delta using some default size distribution law.
    * @param delta the delta to be initialized.
    * @param nChanges the number of possible changes.
    * @param expectedSize the expected delta size.
    * @param rng the random number generator.
    * @return the size of the just-initialized delta.
    */
  def initializeDeltaWithDefaultSize(delta: OrderedSet[ChangeType], nChanges: ChangeSizeType, expectedSize: Double, rng: Random): Int

  /**
    * Initializes the given delta using the specified delta size.
    * @param delta the delta to be initialized.
    * @param nChanges the number of possible changes.
    * @param size the size which the delta must have.
    * @param rng the random number generator.
    */
  def initializeDeltaWithGivenSize(delta: OrderedSet[ChangeType], nChanges: ChangeSizeType, size: Int, rng: Random): Unit

  /**
    * Initializes the given delta using some default distribution law, but taking
    * @param delta the delta to be initialized.
    * @param source the source delta; the initialized delta shall be a subset of the source delta.
    * @param expectedSize the expected delta size.
    * @param rng the random number generator.
    * @return the size of the just-initialized delta.
    */
  def initializeDeltaFromExisting(delta: OrderedSet[ChangeType], source: OrderedSet[ChangeType],
                                  expectedSize: Double, rng: Random): Int
}

/**
  * This companion object contains several known implementations of the `HasDeltaOperations` trait.
  */
object HasDeltaOperations {
  implicit def forInt:  HasDeltaOperations[Int, Int]   = IntSetOps
  implicit def forLong: HasDeltaOperations[Long, Long] = LongSetOps
}
