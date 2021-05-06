package cats.reflect

import java.util.concurrent.Semaphore
import scala.annotation.tailrec

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
  // println {
  //   val N = 1000000
  //   println("--- IO ---")
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }
  //   timed { (reify[IO] in { countDownReflect(N) }).unsafeRunSync() }

  //   println("--- Id ---")
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }
  //   timed { (reify[Id] in { countDownReflectId(N) }) }

  //   println("--- Nested ---")
  //   // it looks like reflect is linear in the number of intermediate delimiters...
  //   inline def nested = (reify [Id] in { 
  //       (reify [IO] in { 
  //         reify [List] in { 
  //           reify [IntWriter] in { 
  //             (reify [IntReader] in { 
  //               countDownReflectId(N) 
  //             }).run(0)
  //           }
  //         }
  //       }).unsafeRunSync() 
  //     })
  //   timed { nested }
  //   timed { nested }
  //   timed { nested }
  //   timed { nested }
  //   timed { nested }
  //   timed { nested }
  //   timed { nested }


  //   println("--- Baseline ---")

  //   // obviously, this can aggressivly be optimized...
  //   timed { countDownBaseline(N) }
  //   timed { countDownBaseline(N) }
  //   timed { countDownBaseline(N) }
  //   timed { countDownBaseline(N) }
  // }
}

import cats.effect.{IO, IOApp}

object IOExamples extends App {

  import cats.effect.FiberIO
  import cats.effect.unsafe.implicits.global
  import scala.concurrent.duration._

  object IO extends RunnerOps[IO] {
    def sleep(delay: FiniteDuration): Unit in IO =
      cats.effect.IO.sleep(delay).reflect

    def start[R](prog: R in IO): FiberIO[R] in IO = 
      IO { prog }.start.reflect
  }

  extension [R](fiber: FiberIO[R])
    def join(): Unit in IO = fiber.join.reflect  

  // computation stays computation, no need to wrap into IO
  def sayHello() = println("Hello")

  def ex1 = { sayHello(); sayHello(); sayHello() }

  def ex2(): Unit in IO = {
      sayHello()
      val fiber = IO.start { IO.sleep(1.second) }
      fiber.join()
      sayHello()
  }

  // IO { ... } : IO[A]
  IO { ex2() }.unsafeRunSync()

}

object RateLimiter extends IOApp.Simple {

  // Rate limiting example from 
  //   https://medium.com/disney-streaming/a-rate-limiter-in-15-lines-of-code-with-cats-effect-af09d838857a
  import cats.effect._
  import cats.syntax.all._
  import cats.effect.std.{ Semaphore }

  import scala.concurrent.duration._

  // original rate limiter code
  def rateLimited[A, B](semaphore : Semaphore[IO], function : A => IO[B]): A => IO[B] = input =>
    for {
      _  <- semaphore.acquire
      timerFiber <- IO.sleep(1.second).start
      result <- function(input)
      _ <- timerFiber.join
      _  <- semaphore.release
      } yield result

  // example translated to direct style
  def rateLimitedDirectStyle[A, B](semaphore : Semaphore[IO], function : A => B in IO): A => B in IO = input => {
    semaphore.acquire.reflect;
    val timerFiber = IO.sleep(1.second).start.reflect;
    val result = function(input);
    timerFiber.join.reflect;
    semaphore.release.reflect;
    result
  }

  // "big" dataset
  val myData : List[Int] = (1 to 30).toList

  def process: List[String] in IO = {
    println("Starting to process!")
    val sem = Semaphore[IO](10).reflect
    val limited = rateLimitedDirectStyle(sem, n => { println(s"hey! ${n}"); n.toString })
    // here we need to locally reify, since parTraverse has type:
    //   def parTraverse[A](as: List[A])(f: A => IO[B]): IO[List[B]]
    myData.parTraverse(n => reify[IO] in { limited(n) }).reflect
  }

  override def run: IO[Unit] = reify[IO] in { println(process) }
}

object InteractionExceptions extends App {

  class MyExc extends Exception

  import cats.data._
  import cats.implicits._

  type IntReader[A] = Reader[Int, A]
  type IntWriter[A] = Writer[Int, A]

  def read(): Int in IntReader = Reader[Int, Int] { n => n }.reflect
  def write(n: Int): Unit in IntWriter = Writer.tell(n).reflect

  val res: Int in IntReader = 
    read() + ((throw new MyExc) : Int) + read()
  

  println("\nReader Example:")
  
  val m: IntReader[Int] = reify [IntReader] in { res }

  // try-catch is like reify, but for exceptions
  val opt = try { Some(m.run(2)) } catch {
    case e: MyExc => None
  }

  println(opt)


}

object CPSExamples extends App {

  type CPS[R] = [A] =>> (A => R) => R

  def shift[A, R](body: (A => R) => R): CPS[R][A] = body

  given [Ans] : Runner[CPS[Ans]] with
      def pure[A](a: A) = k => k(a)

      // this is very obviously NOT tail recursive...
      def tailRecM[X, R](init: CPS[Ans][X])(f: X => Either[CPS[Ans][X], CPS[Ans][R]]): CPS[Ans][R] =
        k => init(x => f(x) match {
          case Left(cx) =>             
            tailRecM(cx)(f)(k)
          case Right(cr) => 
            cr(k)
        })

  def noop[R](): Unit in CPS[R] = 
    shift[Unit, R](k => k(())).reflect

  def overflow[R](): Unit in CPS[R] = {
    var i = 10000
    while (i > 0) {
      noop[R]();
      i = i - 1
    }
  }

  val cps = reify [CPS[Unit]] in {
    overflow()
  }

  // stack overflow
  cps(x => x)
}

object DelimitedControl extends App {

    import cats.reflect.examples.control

    import control.Control

    given Runner[Control] with
      def pure[A](a: A) = control.pure(a)

      // Control already performs trampolining, so we don't use tailrec here.
      // is this a problem?
      def tailRecM[X, R](init: Control[X])(f: X => Either[Control[X], Control[R]]): Control[R] = 
        init.flatMap { x => f(x) match {
          case Left(cx) => tailRecM(cx)(f)
          case Right(res) => res
        }}
    
    val marker = new control.ContMarker[Unit] {}

    def noop(): Unit in Control = 
      Control.use(marker) { k => k(()) } .reflect

    // now this version DOES allocate frames on the metastack
    def noop2(): Unit in Control = 
      Control.use(marker) { k =>
        reify[Control] in {
          // this is a continuation you might want to pass to someAsyncFunction...
          val continuation = () => k(()).reflect
          val res = continuation();
          res
        }
      }.reflect

    def debug(): Unit in Control = 
      Control.printContinuation().reflect

    def overflow1(i: Int): Unit in Control = {      
      if (i > 0) {
        noop2();
        //debug();
        overflow1(i - 1)
      } else {
        () 
      }
    }

    def overflow2(n: Int): Unit in Control = {
      var i = n
      while (i > 0) {
        noop();
        //debug();
        i = i - 1
      }
    }
    
    control.run { Control.delimitCont(marker) { _ => 
      reify [Control] in { overflow1(10000) } 
    }}

    control.run { Control.delimitCont(marker) { _ => 
      reify [Control] in { overflow2(10) } 
    }}
}