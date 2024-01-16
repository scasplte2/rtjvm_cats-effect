package com.rockthejvm.part3concurrency

import cats.effect.{Fiber, IO, IOApp, Outcome}

import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple {

  import com.rockthejvm.utils.*

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] = {
    (
      IO(s"Starting computation: $value").debug >>
        IO.sleep(duration) >>
        IO(s"computation for $value: done") >>
        IO(value)
      ).onCancel(IO(s"computation CANCELED for $value").debug.void)
  }

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
    - both IOS run on separate fibers
    - the first one to fiisih will compelte the result
    - the loser will be canceled
    */

    first.flatMap {
      case Left(value) => IO(s"Meaning of life won: $value")
      case Right(value) => IO(s"Favorite language won: $value")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])  // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outInt, fibStr)) => fibStr.cancel >> IO("Meaning of life won").debug >> IO(outInt).debug
      case Right((fibInt, outStr)) => fibInt.cancel >> IO("Favorite language won").debug >> IO(outStr).debug
    }
  }

  /**
   * Exercises
   * 1 - Implement a timeout pattern with race
   * 2 - a method to return a LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   */

  // 1 - race the two IOs
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    IO.race(IO.sleep(duration), io)
      .flatMap {
        case Left(_) => IO.raiseError(new RuntimeException("Computation timed out"))
        case Right(value) => IO.pure(value)
      }
  }

  val importantTask = IO.sleep(2.seconds) >> IO(42).debug
  val testTimeout = timeout(importantTask, 3.seconds).void
  val testTimeout_v2 = importantTask.timeout(1.second).void

  // 2 -
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob)
      .flatMap {
        case Left((_, fibB)) => fibB.join.flatMap {
          case Outcome.Succeeded(fb) => fb.map(b => Right.apply[A, B](b))
          case Outcome.Errored(e) => IO.raiseError(e)
          case Outcome.Canceled() => IO.raiseError(new RuntimeException("Canceled"))
        }
        case Right((fibA, _)) => fibA.join.flatMap {
          case Outcome.Succeeded(fa) => fa.map(a => Left.apply[A, B](a))
          case Outcome.Errored(e) => IO.raiseError(e)
          case Outcome.Canceled() => IO.raiseError(new RuntimeException("Canceled"))
        }
      }
  }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob)
      .flatMap {
        case Left((outA, fibB)) => outA match {
          case Outcome.Succeeded(fa) => fibB.cancel >> fa.map(a => Left(a))
          case Outcome.Errored(e) => fibB.cancel >> IO.raiseError(e)
          case Outcome.Canceled() => fibB.join.flatMap {
            case Outcome.Succeeded(fb) => fb.map(b => Right(b))
            case Outcome.Errored(e) => IO.raiseError(e)
            case Outcome.Canceled() => IO.raiseError(new RuntimeException("Both effects were canceled"))
          }
        }
        case Right((fibA, outB)) => outB match {
          case Outcome.Succeeded(fb) => fibA.cancel >> fb.map(b => Right(b))
          case Outcome.Errored(e) => fibA.cancel >> IO.raiseError(e)
          case Outcome.Canceled() => fibA.join.flatMap {
            case Outcome.Succeeded(fa) => fa.map(a => Left(a))
            case Outcome.Errored(e) => IO.raiseError(e)
            case Outcome.Canceled() => IO.raiseError(new RuntimeException("Both effects were canceled"))
          }
        }
      }
  }


  override def run = testTimeout_v2

}
