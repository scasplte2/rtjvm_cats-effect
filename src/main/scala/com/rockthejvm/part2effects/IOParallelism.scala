package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {
  // IO are typically sequential
  val anisIO = IO(println(s"[${Thread.currentThread().getName}] Ani"))
  val kamransIO = IO(println(s"[${Thread.currentThread().getName}] Kamran"))

  val composedIO = for {
    ani <- anisIO
    kamran <- kamransIO
  } yield s"$ani and $kamran love Rock the JVM"

  // debug extension method
  import com.rockthejvm.utils._
  import cats.syntax.apply._
  val meaningOfLife = IO.delay(42)
  val favLang = IO.delay("Scala")
  val goalInLife = (meaningOfLife, favLang).mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  import cats.effect.implicits._
  val goalInLifePar = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")
  // turn parallel IO back to sequential
  val goalInLife_v2 = Parallel[IO].sequential(goalInLifePar)

  // shortcuts
  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] = (meaningOfLife.debug, favLang.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  // regarding failure:
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  val paralellWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)
  // compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailure: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
  val twoFailureWithDelay: IO[String] = (aFailure.debug, IO(Thread.sleep(1000)).debug >> anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = twoFailureWithDelay.debug.void

}
