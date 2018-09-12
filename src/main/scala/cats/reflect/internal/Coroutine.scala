package cats.reflect.internal

import java.lang.{ ContinuationScope, Continuation }

trait Prompt[S, R] {
  def suspend(value: S): R
}

class Coroutine[S, R, T](prog: Prompt[S, R] => T) {

  def isDone = co.isDone
  def value: S = { assert(!isDone); receive() }
  def result: T = { assert(isDone); receive() }
  def resume(v: R): Unit = { assert(!isDone); send(v); co.run() }

  private var channel: Any = null
  private def send(v: Any) = channel = v
  private def receive[A](): A = {
    val v = channel
    v.asInstanceOf[A]
  }

  private object prompt extends ContinuationScope("cats-reflect") with Prompt[S, R] {
    def suspend(value: S): R = {
      send(value)
      Continuation `yield` this
      receive()
    }
  }

  private val co = new Continuation(prompt, () => send(prog(prompt)))

  co.run()
}
