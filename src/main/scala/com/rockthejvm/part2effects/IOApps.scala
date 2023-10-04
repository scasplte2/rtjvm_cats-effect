package com.rockthejvm.part2effects

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {
  val program: IO[Unit] = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"You've just written: $line"))
  } yield ()
}

object TestApp {
  import IOApps._

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    program.unsafeRunSync()
  }
}

object MyFirstIOApp extends IOApp {
  import IOApps._
  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}

object MySimpleApp extends IOApp.Simple {
  import IOApps._

  override def run = program
}
