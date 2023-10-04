package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.IO.{IOCont, Uncancelable}

import scala.util.{Failure, Success, Try}

object IOErrorHandling {

  // can directly construct IO with pure, delay, and defer
  // now want to include failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail")) // use this when having to pass exception as result

  // handle exceptions being passed back up
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
  }

  // turn into an Either
    val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
  // redeem: transform the failure and the success in one go
  val resultAsString = aFailure.redeem(ex => s"Fail: $ex", value => s"Success: $value")
  // redeemWith
  val resultAsEffect = aFailure.redeemWith(ex => IO(println(s"Fail: s$ex")), value => IO(println(s"Success: $value")))

  /**
   * Exercies
   */
  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case None => IO.raiseError(ifEmpty)
    case Some(value) => IO.pure(value)
  }
  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Failure(ex) => IO.raiseError(ex)
    case Success(value) => IO.pure(value)
  }

  def either2IO[A](either: Either[Throwable, A]): IO[A] = either match {
    case Left(ex) => IO.raiseError(ex)
    case Right(value) => IO.pure(value)
  }

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO.pure)


  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    dealWithIt.unsafeRunSync()

  }

}
