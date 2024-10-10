package com.rockthejvm.part5polymorphic

import cats.Defer
import cats.effect.{IO, IOApp, MonadCancel, Sync}
import cats.syntax.functor.*

import java.io.{BufferedReader, InputStreamReader}
import scala.io.StdIn

object PolymorphicSync extends IOApp.Simple {

  val aDelayedIO = IO.delay { // "suspend" computations in IO
    println("I'm an effect") // not printed
    42
  }

  val aBlockingIO = IO.blocking { // executed on a specific thread pool for blocking computations
    println("Loading")
    Thread.sleep(1000)
    42
  }

  // synchronous computation is either delayed or blocked computation suspended into an effectful wrapper

  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](thunk: => A): F[A] //"suspension of a computation - will run on the CE thread pool
    def blocking[A](thunk: => A): F[A] // runs on blocking thread-pool

    def defer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)
  }

  val syncIO = Sync[IO] // given Sync[IO] in scape

  // abilities: pure, map/flatMap, raiseError, uncancelable, +delay/blocking
  val aDelayIO_v2 = Sync[IO].delay {
    println("I'm an effect!")
    42
  } // same as aDelayedIO above

  val aBlockingIO_v2 = Sync[IO].blocking {
    println("Loading")
    Thread.sleep(1000)
    42
  }

  val aDeferredIO = IO.defer(aDelayedIO)

  /*
  Exercise - write a polymorphic console
   */

  trait Console[F[_]] {
    def println[A](a: A): F[Unit]
    def readLine(): F[String]
  }

  object Console {
    def apply[F[_] : Sync]: F[Console[F]] = Sync[F].pure((System.in, System.out)).map {
      case (in, out) => new Console[F] {

        override def println[A](a: A): F[Unit] = Sync[F].blocking(out.println(a))

        override def readLine(): F[String] = {
          val bufferedReader = new BufferedReader(new InputStreamReader(in))
          Sync[F].blocking(bufferedReader.readLine())
        }
      }
    }
  }

  def consoleProgram() = for {
    console <- Console[IO]
    _ <- console.println("Hello, what is your name?")
    name <- console.readLine()
    _ <- console.println(s"Nice to meet you $name!")
  } yield ()

  override def run: IO[Unit] = consoleProgram()
}
