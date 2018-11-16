/**
 * Implementation of Effekt, based on cats-reflect.
 * Most of the implementation details are equivalent to
 * upstream-effekt.
 *
 * The types of `using`, `CPS`, `handle`, and `use` changed to
 * accommodate "direct style".
 */
package object effekt {

  import cats.Monad
  import cats.reflect._
  import language.implicitConversions


  // LIBRARY INTERFACE
  // =================

  // this is just a type-carrying marker
  type Cap = AnyRef { type Res }
  type Prompt[R] = Cap { type Res = R }

  type using[A, R] = implicit R => A in Control
  type and[A, R] = implicit R => A
  type CPS[A, E] = implicit (A => E in Control) => E in Control

  trait HandlerPrompt[Res0] { type Res = Res0 }

  // We don't have unit/pure/return anymore...
  trait Handler[Res0] {
    // the effect domain
    type Res = Res0
    def use[A](body: CPS[A, Res]): A in Control = effekt.use(this)(body)
  }


  implicit object controlMonad extends Monad[Control] {
    def pure[A](x: A) = new Trivial(x)
    def flatMap[A, B](c: Control[A])(f: Frame[A, B]) = c flatMap f
    // control itself is trampolining, so not necessary here
    def tailRecM[A, B](init: A)(step: A => Control[Either[A, B]]): Control[B] =
      step(init) flatMap {
        case Left(a) => tailRecM(a)(step)
        case Right(b) => pure(b)
      }
  }

  implicit def asPrompt[T <: AnyRef, R](a: T): T & Prompt[R] =
    a.asInstanceOf[T & Prompt[R]]

  final def resume[A, Res](a: A): CPS[A, Res] = implicit k => k(a)
  final def resume[A, Res, S](a: A, s: S)(implicit k: ((A, S) => Res in Control)): Res in Control = k(a, s)

  // More low-level capability based control operators

  def handle[C <: Cap](p: C)(f: p.Res using p.type): p.Res in Control =
    new Control[p.Res] {
      def apply[R2](k: MetaCont[p.Res, R2]): Result[R2] = {
        Impure(reify[Control] in { f(p) }, PromptCont[p.Res, R2](p, k))
      }
    }.reflect

  def use[A](p: Cap)(body: CPS[A, p.Res]): A in Control =
    new Control[A] {
      def apply[R](k: MetaCont[A, R]): Result[R] = {

        val (init, tail) = k splitAt p

        val handled: p.Res in Control = body { a =>
          new Control[p.Res] {
            def apply[R2](k: MetaCont[p.Res, R2]): Result[R2] =
              (init append PromptCont(p, k)).apply(a)
          }.reflect
        }

        // continue with tail
        Impure(reify[Control] in { handled }, tail)
      }
    }.reflect


  // IMPLEMENTATION DETAILS
  // ======================

  sealed trait Control[+A] { outer =>

    def apply[R](k: MetaCont[A, R]): Result[R]

    def run(): A = Result.trampoline(apply(ReturnCont(identity)))

    def map[B](f: A => B): Control[B] = new Control[B] {
      def apply[R](k: MetaCont[B, R]): Result[R] = outer(k map f)
    }

    def flatMap[B](f: A => Control[B]): Control[B] = new Control[B] {
      def apply[R](k: MetaCont[B, R]): Result[R] = outer(k flatMap f)
    }

    def withFilter(p: A => Boolean): Control[A] = flatMap {
      case a if p(a) => new Trivial(a)
      case a => new Error(new Throwable("Could not match " + a))
    }
  }
  final class Trivial[+A](a: => A) extends Control[A] {
    def apply[R](k: MetaCont[A, R]): Result[R] = k(a)
    override def map[B](f: A => B): Control[B] = new Trivial(f(a))
    override def run(): A = a
  }
  final class Error(t: Throwable) extends Control[Nothing] {
    def apply[R](k: MetaCont[Nothing, R]): Result[R] = Abort(t)
    override def map[B](f: Nothing => B): Control[B] = this
    override def flatMap[B](f: Nothing => Control[B]): Control[B] = this
  }

  type Frame[-A, +B] = Function[A, Control[B]]

  sealed trait Result[+A]
  case class Pure[+A](value: A) extends Result[A]
  case class Impure[+A, R](c: Control[R], k: MetaCont[R, A]) extends Result[A]
  case class Abort(t: Throwable) extends Result[Nothing]

  object Result {
    @scala.annotation.tailrec
    def trampoline[A](y: Result[A]): A = y match {
      case Pure(a) => a
      case Impure(c, k) => trampoline[A](c(k))
      case Abort(t) => throw t
    }
  }

  sealed trait MetaCont[-A, +B] extends Serializable {
    def apply(a: A): Result[B]
    def append[C](s: MetaCont[B, C]): MetaCont[A, C]
    def map[C](f: C => A): MetaCont[C, B] = flatMap(x => new Trivial(f(x)))
    def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FramesCont(List(f), this)
    def splitAt(c: Cap): (MetaCont[A, c.Res], MetaCont[c.Res, B]) =
      sys error s"Prompt $c not found on the stack."
  }

  case class ReturnCont[-A, +B](f: A => B) extends MetaCont[A, B] {
    final def apply(a: A): Result[B] = Pure(f(a))
    final def append[C](s: MetaCont[B, C]): MetaCont[A, C] = s map f
  }

  case class FramesCont[-A, B, +C](frames: List[Frame[Nothing, Any]], tail: MetaCont[B, C]) extends MetaCont[A, C] {

    final def apply(a: A): Result[C] = {
      val first :: rest = frames
      val result = first.asInstanceOf[Frame[A, B]](a)
      if (rest.isEmpty) {
        Impure(result, tail)
      } else {
        Impure(result, FramesCont(rest, tail))
      }
    }

    final def append[D](s: MetaCont[C, D]): MetaCont[A, D] = FramesCont(frames, tail append s)
    override def flatMap[D](f: Frame[D, A]): MetaCont[D, C] = FramesCont(f :: frames, tail)
    override final def splitAt(c: Cap) = tail.splitAt(c) match {
      case (head, tail) => (FramesCont(frames, head), tail)
    }
  }

  case class PromptCont[R, B](cap: Prompt[R], tail: MetaCont[R, B]) extends MetaCont[R, B] {
    final def apply(r: R): Result[B] = tail(r)
    final def append[C](s: MetaCont[B, C]): MetaCont[R, C] = PromptCont(cap, tail append s)
    override final def splitAt(c: Cap) =

      // We found the corrsponding handler!
      // ---
      // Here we deduce type equality from referential equality
      // that is R==c.Res
      if (cap eq c) {
        val head = ReturnCont[R, c.Res](x => x.asInstanceOf[c.Res])
        val rest = tail.asInstanceOf[MetaCont[c.Res, B]]
        (head, rest)

      // Not the right handler
      // ---
      // remove cleanup from this handler and prepend to found handler.
      // this way we assert that the cleanup actions will be called, even
      // if the continuation is discarded in the handler implementation.
      } else tail.splitAt(c) match {
        case (head, tail) =>
        (PromptCont(cap, head), tail)
      }
  }
}
