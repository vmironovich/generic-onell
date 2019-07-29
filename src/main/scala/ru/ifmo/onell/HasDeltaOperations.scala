package ru.ifmo.onell

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.delta.IntSetOps
import ru.ifmo.onell.util.IntSet

/**
  * This trait encapsulates the knowledge about manipulations with delta representations.
  *
  * @tparam DeltaRepresentation the type of the delta representation.
  */
trait HasDeltaOperations[DeltaRepresentation] {
  /**
    * Creates a new delta representation, given the number of possible changes.
    * @param nChanges the number of possible changes.
    * @return the newly created delta representation.
    */
  def createStorage(nChanges: Long): DeltaRepresentation

  /**
    * Initializes the given delta using some default size distribution law.
    * @param delta the delta to be initialized.
    * @param nChanges the number of possible changes.
    * @param expectedSize the expected delta size.
    * @param rng the random number generator.
    * @return the size of the just-initialized delta.
    */
  def initializeDeltaWithDefaultSize(delta: DeltaRepresentation, nChanges: Long, expectedSize: Double, rng: Random): Int

  /**
    * Initializes the given delta using the specified delta size.
    * @param delta the delta to be initialized.
    * @param nChanges the number of possible changes.
    * @param size the size which the delta must have.
    * @param rng the random number generator.
    */
  def initializeDeltaWithGivenSize(delta: DeltaRepresentation, nChanges: Long, size: Int, rng: Random): Unit

  /**
    * Initializes the given delta using some default distribution law, but taking
    * @param delta the delta to be initialized.
    * @param source the source delta; the initialized delta shall be a subset of the source delta.
    * @param expectedSize the expected delta size.
    * @param rng the random number generator.
    * @return the size of the just-initialized delta.
    */
  def initializeDeltaFromExisting(delta: DeltaRepresentation, source: DeltaRepresentation,
                                  expectedSize: Double, rng: Random): Int

  /**
    * Copies the deltas from source to target.
    * @param source the source delta.
    * @param target the target delta.
    */
  def copyDelta(source: DeltaRepresentation, target: DeltaRepresentation): Unit
}

/**
  * This companion object contains several known implementations of the `HasDeltaOperations` trait.
  */
object HasDeltaOperations {
  implicit def forIntArraySet: HasDeltaOperations[IntSet] = IntSetOps
}
