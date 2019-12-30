package ru.ifmo.onell.util

/**
  * This is the utility object which shares common knowledge about code specialization.
  */
object Specialization {
  final val fitnessSpecialization = Specializable.Bits32AndUp
  final val changeSpecialization: Specializable.Group[(Int, Long)] = null
}
