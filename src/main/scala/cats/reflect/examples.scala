package cats.reflect

package object examples extends App {

  import cats.data._
  import cats.implicits._

  type IntReader[A] = Reader[Int, A]
  type IntWriter[A] = Writer[Int, A]

  def read(): Int in IntReader = Reader[Int, Int] { n => n }.reflect

  def write(n: Int): implicit Reflect[IntWriter] => Unit =
    implicit r => Writer.tell(n).reflect

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

  val resBoth: IntReader[IntWriter[Unit]] = reify [IntReader] in { implicit reader =>
    reify [IntWriter] in { implicit writer =>
      write(read()(reader))(writer) // +1

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

  val ioa: Unit in IO = IO { println("hey!") }.reflect

  val program: Unit in IO = {
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
    (reify [IO] in { fib(100000) }).unsafeRunSync()
  }
}
