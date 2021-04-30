package cats
package reflect

// This is really just a monad...
trait Runner[M[_]] {
  def pure[A](a: A): M[A]
  
  /**
   * This is our own very special version of tailRecM
   * 
   * Actually the type X can be different for every call to f...
   */
  def tailRecM[X, R](init: M[X])(f: X => Either[M[X], M[R]]): M[R]
}

trait RunnerOps[M[_]: Runner] {
  def apply[R](prog: R in M): M[R] = cats.reflect.reify[M, R](prog)
}