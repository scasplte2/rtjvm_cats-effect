package com.rockthejvm.part4coordination

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel._
import scala.concurrent.duration._

object Refs extends IOApp.Simple {

  import com.rockthejvm.utils._

  // ref - purely functional atomic reference
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2_v2: IO[Ref[IO, Int]] = IO.ref(42)

  // modifying is an effect
  val increaseMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(43) // always thread-safe
  }

  // obtain a value
  val mol = atomicMol.flatMap { ref =>
    ref.get // thread-safe
  }

  val gsMol = atomicMol.flatMap { ref =>
    ref.getAndSet(43)
  } // gets the old value, sets the new one

  // updating with a function
  val fMol = atomicMol.flatMap { ref =>
    ref.update(value => value * 10)
  }

  val updatedMol = atomicMol.flatMap { ref =>
    ref.updateAndGet(value => value * 10) // get the new value
    // can also use getAndUpdate to get the OLD value
  }

  // modifying with a function returning a different type
  val modifiedMol = atomicMol.flatMap { ref =>
    ref.modify(value => (value * 10, s"my current value is $value"))
  }

  // why: concurrent + thread-safe reads/writes over shared values, in a purely funtional way
  def demoConcurrentWorkImpure(): IO[Unit] = {

    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").debug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").debug
        _ <- IO(count += wordCount)
      } yield ()
    }

    List("I love Cats Effect", "This ref thing is useless", "James writes a lot of code")
      .map(task)
      .parSequence
      .void
  }
  /**
   * Drawbacks to the above approach
   * - hard to read/debug
   * - mixing pure and impure code, eliminates the reason for using CE
   * - NOT THREAD SAFE
   */

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").debug
        newCount <- total.updateAndGet(currentCount => currentCount + wordCount)
        _ <- IO(s"New total: $newCount").debug
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("I love Cats Effect", "This ref thing is useless", "James writes a lot of code")
        .map(s => task(s, initialCount))
        .parSequence
    } yield ()
  }

  /**
   * Exercise
   * Refactor impure impl to pure method using Ref
   */
  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1) // not thread-safe
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.second)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_ + 1) // thread-safe
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.second)
      value <- ticks.get
      _ <- IO(s"TICKS: $value").debug
      _ <- printTicks(ticks)
    } yield ()

    for {
      initial <- Ref[IO].of(0)
      _ <- (tickingClock(initial), printTicks(initial)).parTupled
    } yield ()
  }

  def tickingClockWeird(): IO[Unit] = {
    val ticks = Ref[IO].of(0) // IO[ref]

    def tickingClock: IO[Unit] = for {
      t <- ticks // at this point, a NEW Ref effect occurs
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- t.update(_ + 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks // at this point, a NEW Ref effect occurs. This creates a new instance of the Ref
      _ <- IO.sleep(5.second)
      currentTicks <- t.get
      _ <- IO(s"TICKS: $currentTicks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }


  override def run = tickingClockWeird()

}
