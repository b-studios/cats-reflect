package cats.reflect.examples

package object control {
  
  // Markers
  // ===
  // previously called "Prompt"
  sealed trait Marker

  /**
   * Marker used on the stack to delimit captured continuations
   */
  @scala.annotation.implicitNotFound("No continuation marker found for 'use'. Maybe you forgot to handle the effect?")
  trait ContMarker[Res] extends Marker

  /**
   * Marker used on the stack to store ambient state (delimited dynamic state)
   */
  trait StateMarker extends Marker {
    type StateRep
    def backup: StateRep
    def restore(value: StateRep): Unit
  }

  /**
   * Marker used on the stack to store exception handlers for interaction with native exceptions
   */
  trait CatchMarker[Res] extends Marker {
    def _catch: PartialFunction[Throwable, Control[Res]]
  }


  // Aliases
  // ===

  type Frame[-A, +B] = A => Control[B]

  type CPS[+A, E] = (A => Control[E]) => Control[E]


  // Main API
  // ===

  final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def run[A](c: Control[A]): A = c.run()
}