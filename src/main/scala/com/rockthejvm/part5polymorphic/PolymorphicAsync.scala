package com.rockthejvm.part5polymorphic

import cats.effect.kernel.Async
import cats.effect.{Concurrent, IO, IOApp, Sync, Temporal}
import com.rockthejvm.utils.*
import cats.syntax.all._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object PolymorphicAsync extends IOApp.Simple {

  type Callback[A] = Either[Throwable, A] => Unit

  // Async - "suspends" asynchronous computations in F
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    // fundamental operations
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]

    // never-ending effect
    def never[A]: F[A] = async(_ => pure(None))

    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] = async(kb => map(pure(cb(kb)))(_ => None))

  }

  val asyncIO = Async[IO] // summons implicit async in scope

  //abilities: pure, map/flatMap, raiseError, uncancelable, start, ref/deffered, sleep, delay/defer/blocking, +...
  // very powerful typeclass (can launch rockets)
  val ec = Async[IO].executionContext

  // powerful: async_ + async
  // foreign-function interface - suspended computation from some external thread pool into a thread pool managed by CE
  val threadPool = Executors.newFixedThreadPool(10)
  val AsyncMeaningOfLife = Async[IO].async_ { (cb: Callback[Int]) =>
    // start computation on some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] Computing an async MOL")
      cb(Right(42))
    }
  }

  val asyncMeaningOfLifeComplex: IO[Int] = Async[IO].async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] Computing an async MOL")
        cb(Right(42))
      }
    }.as(Some(IO("Cancelled!").debug.void)) // <- finalizer in case the computation is cancelled
  }

  val myExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  val asyncMeaningOfLife_v3 = asyncIO.evalOn(IO(42).debug, myExecutionContext).guarantee(IO(threadPool.shutdown()))

  // never
  val neverIO = Async[IO].never

  /*
  Exercises
  1 - implement never and async_ in terms of async
  2 - tuple two effects with different requirements
   */
  def firstEffect[F[_]: Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)
  def secondEffect[F[_]: Sync, A](a: A): F[A] = Sync[F].pure(a)

  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] = for {
    first <- firstEffect(a)
    second <- secondEffect(a)
  } yield (first, second)

  override def run: IO[Unit] = ???
}
