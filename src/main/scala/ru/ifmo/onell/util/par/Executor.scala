package ru.ifmo.onell.util.par

trait Executor extends AutoCloseable {
  def addTask(fun: => String): Unit
}
