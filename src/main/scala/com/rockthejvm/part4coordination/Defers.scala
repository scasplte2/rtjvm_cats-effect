package com.rockthejvm.part4coordination

import cats.effect.{Deferred, Fiber, IO, IOApp, Ref, Outcome}

import scala.concurrent.duration.*
import cats.syntax.traverse.*

object Defers extends IOApp.Simple {

  import com.rockthejvm.utils._

  // deferred is a primitive for waiting for an effect, while some other effect completes with a value

  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  // get blocks the calling fiber (semantically) until some other fiber completes the Deferred with a value
  val reader = aDeferred_v2.flatMap { deferred =>
    deferred.get // blocks the fiber
  }

  val writer = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  // deferred is practically useful in a dual environment (client-server, p2p, etc)
  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result...").debug
      meaningOfLife <- signal.get // blocker
      _ <- IO(s"[consumer] got the result: $meaningOfLife").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO(s"[producer] crunching numbers...").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").debug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibConsumer.join
      _ <- fibProducer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.traverse { part =>
        IO(s"[downloader] got '$part'").debug >> IO.sleep(1.second) >> contentRef.update(currentContent => currentContent + part)
      }.void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("[notifier] File download complete").debug
      else IO(s"[notifier] downloading...").debug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()
  }

  // refactor with Deferred
  def fileNotifierWithDeferred(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = {
      fileParts.traverse { part =>
        fileDownloader(part, contentRef, signal)
      }.void
    }

    def notifyFileComplete(contentRef: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[notifier] downloading...").debug
      _ <- contentRef.get
      _ <- IO("[notifier] File download complete").debug
    } yield ()

    def fileDownloader(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] got '$part'").debug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentContent => currentContent + part)
      _ <- if (latestContent.endsWith("<EOF>")) signal.complete(latestContent) else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifier <- notifyFileComplete(signal).start
      fibDownloader <- downloadFile(contentRef, signal).start
      _ <- notifier.join
      _ <- fibDownloader.join
    } yield ()
  }

  /**
   * Exercises
   * - write an alarm notification with two simultaneous IOs
   *   - one that increments a counter ever second (a clock)
   *   - one that waits for the couter to become 10, then prints a message "time's up"
   *
   * - implement racePair with Deferred
   *   - use a deferred which can hold an Either[outcome for ioa, outcome for iob]
   *   - start two fibers, one for each IO
   *   - on completion (with any status), each IO needs to complete that Deferred
   *     (hint: use a finalizer from the Resources lesson)
   *     (hint 2: use a guarantee call to make sure the fibers complete the Deferred)
   *   - what do you do in the case of cancellation (the hardest part)?
   */

  // deferred is practically useful in a dual environment (client-server, p2p, etc)
  def tenSecondAlarm(): IO[Unit] = {
    def notify(signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO(s"[alarm] timer started...").debug
      _ <- signal.get
      _ <- IO("[alarm] Time's Up!").debug
    } yield ()

    def tickingClock(tickRef: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      tick <- tickRef.updateAndGet(_ + 1)
      _ <- IO(s"[clock] tick: $tick").debug
      _ <- if (tick >= 10) signal.complete(()) else tickingClock(tickRef, signal)
    } yield ()

    for {
      tickRef <- Ref[IO].of(0)
      signal <- Deferred[IO, Unit]
      notifier <- notify(signal).start
      clock <- tickingClock(tickRef, signal).start
      _ <- notifier.join
      _ <- clock.join
    } yield ()
  }

  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]),
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])
  ]

  type EitherOutcome[A,B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]]
      fibA <- ioa.guaranteeCase(outA => signal.complete(Left(outA)).void).start
      fibB <- iob.guaranteeCase(outB => signal.complete(Right(outB)).void).start
      res <- poll(signal.get).onCancel { // blocking call - should be cancellable
        for {
          cancelFibA <- fibA.cancel.start
          cancelFibB <- fibB.cancel.start
          _ <- cancelFibA.join
          _ <- cancelFibB.join
        } yield ()
      }
    } yield res match {
      case Left(outA) => Left((outA, fibB))
      case Right(outB) => Right(fibA, outB)
    }
  }

  override def run = ourRacePair(IO.sleep(1.second) >> IO("task 1").debug, IO.sleep(2.second) >> IO("task 2").debug).debug.void

}
