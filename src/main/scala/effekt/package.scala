import java.lang.{ Continuation, ContinuationScope }

import scala.annotation.tailrec
import scala.util.DynamicVariable

// TODO
// - [ ] Explore exceptions to improve public API as
//         try handle { } catch { case Op(n) -> k => ??? }
//       but then we also need to incorporate return clauses as operations.
//       How to type-check that?
//       Also we need to reinstantiate the handler in the continuation, that's
//       tricky if it is not part of the call to `handle`.
//
// - [ ] Make implementation stack safe

package object effekt {

  // PUBLIC API
  // ==========

  trait Effect[R] {
    def unary_! : R = {
      if (currentPrompt.value == null) {
        sys error s"Unhandled operation ${this}"
      }
      currentPrompt.value.send(this)
    }
  }

  object -> {
    def unapply[A, X](c: Clause[A]): Option[(Effect[X], X => A)] = c match {
      case Bind(op: Effect[X], k) => Some(op, k)
      case _ => None
    }
  }

  sealed trait Clause[R]

  type Handler[Res] = PartialFunction[Clause[Res], Res]

  def handle[R](prog: => R): HandleApi[R, R] = HandleApi(() => prog, r => r)

  def handler[R, Res](unit: R => Res)(h: Handler[Res]): (=> R) => Res = prog =>
    handle { prog } returning unit using h

  def handler[Res](h: Handler[Res]): (=> Res) => Res = prog =>
    handle { prog } using h

  case class HandleApi[R, Res](prog: () => R, unit: R => Res) {
    def returning[S](f: Res => S) = HandleApi(prog, unit andThen f)
    def using(h: Handler[Res]): Res = new Reset[R, Res](unit, h, prog).run()
  }


  // IMPLEMENTATION DETAILS
  // ======================

  private case class Bind[X, R](o: Effect[X], k: X => R) extends Clause[R]

  private object channel {
    private val data = new ThreadLocal[Any]
    def send(v: Any) = data.set(v)
    def receive[A](): A = data.get.asInstanceOf[A]
  }
  private val currentPrompt = new DynamicVariable[Prompt](null)

  private class Prompt extends ContinuationScope("Prompt") {
    def send[X](op: Effect[X]): X = {
      channel.send(op)
      Continuation `yield` this
      channel.receive()
    }
  }

  private class Reset[R, Res](unit: R => Res, h: Handler[Res], prog: () => R) {

    private object prompt extends Prompt

    private lazy val co = new Continuation(prompt, () => channel.send(prog()))

    // The coroutine keeps sending op until it completes
    def run(): Res = {
      currentPrompt.withValue(prompt) {
        co.run()
      }

      if (co.isDone()) {
        return unit(channel.receive())
      }

      Bind(channel.receive(), x => { channel.send(x); run() }) match {
        case c if h.isDefinedAt(c) => h(c)
        case Bind(op, k) => k(!op) // forward
      }
    }
  }
}

object test extends App {
  import effekt._

  case object Get extends Effect[Int]
  case class Out(n: Int) extends Effect[Unit]

  val r1 = handle {
    handle {
      var x = 10
      !Get
      while (x > 0) {
        !Out(x)
        x -= 1
      }
      !Out(!Get + !Get)
      !Out(2);
    } using {
      case Get -> k => k(42)
    }

    !Out(3);
  } returning {
    _ => List.empty[Int]
  } using {
   case Out(n) -> k => n :: k(())
  }

  println(r1)

  // since we can only resume once, it is safe to use mutable state
  def incr[R] = {
    var count = 1
    handler [R] { case Get -> k => count += 1; k(count) }
  }

  val r2 = incr {
    handle {
      !Get
    } returning {
      !Get + _
    } using {
      case Get -> k => k(!Get + 1)
    }
  }

  assert(r2 == 6)

  // Translating some examples of:
  //   https://github.com/ocamllabs/ocaml-effects-tutorial

  class State[S] {
    // we can make effects private so they can only be handled by this
    // very handler
    private case object Get extends Effect[S]
    private case class Put(s: S) extends Effect[Unit]

    def get() = !Get
    def put(s: S) = !Put(s)

    def run[R](init: S)(prog: => R) = (handler[R, S => R] (r => s => r) {
      case Get -> k    => s => k(s)(s)
      case Put(s) -> k => _ => k(())(s)
    })(prog)(init)

    def runStateful[R](init: S)(prog: => R) = {
      var state = init
      handle { prog } using {
        case Get -> k => k(state)
        case Put(s) -> k => state = s; k(())
      }
    }
  }

  object S1 extends State[Int]
  object S2 extends State[String]

  def inc() = S1.put(S1.get() + 1)

  S1.run(0) {
    S2.run("hello") {
      println("before: " + S1.get())
      inc(); inc();
      S2.put(S2.get() + " world")
      inc();
      println("after incrementing three times: " + S1.get())
      println(S2.get())
    }
  }

  class Exceptions {
    case class Raise(msg: String) extends Effect[Nothing]

    def raise(msg: String) = !Raise(msg)

    def apply[R](prog: => R) = ExceptionApi[R](() => prog)

    case class ExceptionApi[R](prog: () => R) {
      def handle(h: String => R): R = effekt.handle { prog() } using {
        case Raise(msg) -> k => h(msg)
      }
    }
  }
  object MyException extends Exceptions

  S1.run(0) {
    MyException {
      S2.run("hello") {
        println("before: " + S1.get())
        inc();
        inc();
        S2.put(S2.get() + " world")
        MyException.raise("abort!")
        inc();
        println("after incrementing three times: " + S1.get())
        println(S2.get())
      }
    } handle {
      msg => println("Aborted with: " + msg + "\nS1 is: " + S1.get())
    }
  }

  case class Exchange(in: Int) extends Effect[Int]

  sealed trait Status
  case object Done extends Status
  case class Paused(msg: Int, k: Int => Status) extends Status

  def step[A, B](f: A => B, v: A) =
    handle { f(v) }.returning[Status] { _ => Done } using {
      case Exchange(in) -> k => Paused(in, k)
    }

  @tailrec
  def runBoth(a: => Status)(b: => Status): Unit = (a, b) match {
    case (Done, Done) => ()
    case (Paused(v1, k1), Paused(v2, k2)) => runBoth(k1(v2))(k2(v1))
    case _ => sys error "improper synchronization"
  }

  def channel(name: String): Int => Unit = {
    case 0 => ()
    case n =>
      println(s"$name: sending $n")
      val v = !Exchange(n)
      println(s"$name: received $v")
      channel(name)(n - 1)
  }

  runBoth { step(channel("a"), 3) } { step(channel("b"), 3) }

  S1.runStateful(100) {
    while (S1.get() > 0) {
      S1.put(S1.get() - 1)
    }
  }
}