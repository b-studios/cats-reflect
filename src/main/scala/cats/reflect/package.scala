package cats

import scala.annotation.implicitNotFound

package object reflect {

  type in[A, M[_]] = given Reflect[M] => A

  @implicitNotFound("This expression requires the capability to reify ${M}\nbut cannot find Reflect[${M}] in the current scope.\n\nMaybe you forgot to wrap this expression in a call to:\n    reify [${M}] in { EXPR }")
  sealed trait Reflect[M[_]] {
    def (mr: M[R]) reflect[R](): R
  }

  /**
   * for partially applying type arguments and better type inference
   *
   *   reify [M] in { BLOCK }
   *
   * @usecase def reify[M[_]: Monad] in[R](prog: => R): M[R]
   */
  def reify[M[_]: Monad]: ReifyBuilder[M] = ReifyBuilder()

  case class ReifyBuilder[M[_]: Monad]() {
    def in[R](prog: R in M) = reify[M, R] { prog }
  }


  // this method is private since overloading and partially applying
  // type parameters conflicts and results in non-helpful error messages.
  //
  // tradeoff of using `reify[M] in BLOCK` syntax over this function:
  //   + type inference on R
  //   - no type inference on M
  // The latter might be a good thing since we want to make explicit
  // which monad we are reifying.
  private def reify[M[_], R](prog: R in M) given (M: Monad[M]): M[R] = {
    import internal._

    type X

    // The coroutine keeps sending monadic values until it completes
    // with a monadic value
    val coroutine = new Coroutine[M[X], X, M[R]](prompt => {
      // capability to reflect M
      object reflect extends Reflect[M] {
        def (mr: M[R]) reflect[R](): R =
          // since we know the receiver of this suspend is the
          // call to flatMap, the casts are safe
          prompt.suspend(mr.asInstanceOf[M[X]]).asInstanceOf[R]
      }
      M.pure(prog given reflect)
    })

    def step(x: X): Either[M[X], M[R]] = {
      coroutine.resume(x)
      if (coroutine.isDone)
        Right(coroutine.result)
      else
        Left(coroutine.value)
    }

    def run(): M[R] =
      if (coroutine.isDone)
        coroutine.result
      else
        M.flatten(M.tailRecM(coroutine.value) { mx => M.map(mx) { step }})

    run()
  }
}
