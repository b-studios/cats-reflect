package cats.reflect

package object examples extends App {

  import cats.data._
  import cats.implicits._

  type IntReader[A] = Reader[Int, A]
  type IntWriter[A] = Writer[Int, A]

  def read(): Int in IntReader = Reader[Int, Int] { n => n }.reflect
  def write(n: Int): Unit in IntWriter = Writer.tell(n).reflect

  val res: IntReader[Int] = reify [IntReader] in {
    read() + read() + read()
  }

  println("\nReader Example:")
  println { res.run(2) }

  val res2 = reify [IntWriter] in {
    write(1)
    write(4)
    write(3)
  }

  println("\nWriter Example:")
  println { res2.run(0) }

  def locally[R](n: Int)(prog: => R in IntReader): R =
    reify [IntReader] in { prog } run n

  val resBoth: IntReader[IntWriter[Unit]] = reify [IntReader] in {
    reify [IntWriter] in {
      write(read()) // +1

      Writer(4, ()).reflect

      // locally(read() + 2) { write(read()) } // +3
      write(read()) // +1
    }
  }

  println("\nCombined Example:")
  println { resBoth(1).run(0) }

  // examples from https://typelevel.org/cats-effect/datatypes/io.html
  // translated to use cats-reflect
  import cats.effect.IO
  import cats.effect.unsafe.implicits.global

  def ioa: Unit in IO = IO { println("hey!") }.reflect

  def program: Unit in IO = {
    ioa
    ioa
  }

  println("\nIO Example:")

  (reify [IO] in { program }).unsafeRunSync()

  // cats effect IO trampolining works:
  def fib(n: Int, a: Long = 0, b: Long = 1): Long in IO = {
    val b2 = IO(a + b).reflect

    if (n > 0)
      fib(n - 1, b, b2)
    else
      b2
  }

  def fib2(n: Int, a: Long = 0, b: Long = 1): IO[Long] = reify [IO] in {
    val b2 = IO(a + b).reflect

    if (n > 0)
      fib2(n - 1, b, b2).reflect
    else
      b2
  }

  println("\nFib Example:")
  println {
    (reify [IO] in { fib(6) }).unsafeRunSync()
  }

  def countDownBaseline(n: Int): Long = {
    val b2 = n + n

    if (n > 0)
      countDownBaseline(n - 1)
    else
      b2
  }

  def countDownReflect(n: Int): Long in IO = {
    val b2 = IO(n + n).reflect

    if (n > 0)
      countDownReflect(n - 1)
    else
      b2
  }

  import cats.{ Monad, Id }
  import cats.catsInstancesForId
  // import cats.implicits._

  def countDownReflectId(n: Int): Long in Id = {
    val b2 = Monad[Id].pure(n + n).reflect

    if (n > 0)
      countDownReflectId(n - 1)
    else
      b2
  }

  def countDownReify(n: Int): IO[Long] = reify [IO] in {
    val b2 = IO(n + n).reflect

    if (n > 0)
      countDownReify(n - 1).reflect
    else
      b2
  }

  // some very preliminary benchmarks
  def timed(block: => Unit): Long = {
    val before = System.currentTimeMillis()
    block
    val after = System.currentTimeMillis()
    println(s"took ${after - before}ms")
    after - before
  }

  // this is a quick measurement of .reflect
  println("\nCountdown Reflect Example:")
  println {
    val N = 1000000
    println("--- IO ---")
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
    timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }

    println("--- Id ---")
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }
    timed { (reify[Id] in { countDownReflectId(N) }) }

    println("--- Nested ---")
    // it looks like reflect is linear in the number of intermediate delimiters...
    inline def nested = (reify [Id] in { 
        (reify [IO] in { 
          reify [List] in { 
            reify [IntWriter] in { 
              (reify [IntReader] in { 
                countDownReflectId(N) 
              }).run(0)
            }
          }
        }).unsafeRunSync() 
      })
    timed { nested }
    timed { nested }
    timed { nested }
    timed { nested }
    timed { nested }
    timed { nested }
    timed { nested }


    println("--- Baseline ---")

    // obviously, this can aggressivly be optimized...
    timed { countDownBaseline(N) }
    timed { countDownBaseline(N) }
    timed { countDownBaseline(N) }
    timed { countDownBaseline(N) }
  }
}
