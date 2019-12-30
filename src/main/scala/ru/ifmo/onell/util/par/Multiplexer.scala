package ru.ifmo.onell.util.par

class Multiplexer(base: Executor, times: Int) extends Executor {
  override def close(): Unit = {}
  override def addTask(fun: => String): Unit = {
    for (_ <- 0 until times) {
      base.addTask(fun)
    }
  }
}
