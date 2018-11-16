package effekt
object example extends App {

  import cats.reflect._

  // Effect Signatures
  trait Amb {
    def flip(): Boolean in Control
  }
  def Amb: Amb using Amb = implicit amb => amb

  trait Exc {
    def raise[A](msg: String): A in Control
  }
  def Exc: Exc using Exc = implicit exc => exc

  // Effect Usage
  def drunkFlip(): String using Amb and Exc = {
    if (Amb.flip())
      Exc.raise("too drunk to flip a coin...")
    else if (Amb.flip())
      "heads"
    else
      "tails"
  }


  // Effect Handlers

  // we can't define ambList currently since loom only supports
  // one shot continuations...
  //
  // Also, there is a change in API: handler classes _always_
  // only fix their effect domain (here List[R]).
  // There is no "pure" or "return" clause anymore. Instead
  // it is inlined in the call to "handle" below. This has
  // the advantage that handlers with the same effect domain
  // can just be composed without conflicting "return" clauses.
  // It also simplifies the API.
  trait Random[R] extends Amb with Handler[List[R]] {
    def flip() = use { resume(Math.random > 0.5) }
  }
  def Random[R](prog: R using Amb): List[R] in Control =
    handle(new Random[R] {}) { List(prog) }

  trait Maybe[R] extends Exc with Handler[Option[R]] {
    def raise[A](msg: String) = use { None }
  }
  def Maybe[R](prog: R using Exc): Option[R] in Control =
    handle(new Maybe[R] {}) { Some(prog) }

  def res: List[Option[String]] in Control =
    Random {
      Maybe {
        drunkFlip()
      }
    }

  println {
    (reify[Control] in res).run()
  }
}
