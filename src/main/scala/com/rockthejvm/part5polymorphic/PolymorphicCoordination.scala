package com.rockthejvm.part5polymorphic

import cats.effect.{Concurrent, Deferred, Fiber, IO, IOApp, Outcome, Ref, Spawn}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import com.rockthejvm.part4coordination.MyMutex.State
import com.rockthejvm.utils.general.*

import scala.collection.immutable.Queue
import scala.concurrent.duration.*

object PolymorphicCoordination extends IOApp.Simple {

  // Concurrent - Ref + Deferred for ANY effect type
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO] // implicit instance of Concurrent[IO]
  val aDeferred = Deferred[IO, Int] // requires given Concurrent[IO] in scope
  val aDeferred_v2 = concurrentIO.deferred[Int]
  val aRef = concurrentIO.ref(42)

  // capabilities: pure, map/flatMap, raiseError, uncancelable, start (fibers), + ref/deferred

  def tenSecondAlarm[F[_]: Concurrent]: F[Unit] = {
    def unsafeSleep(duration: FiniteDuration): F[Unit] =
      Concurrent[F].pure(Thread.sleep(duration.toMillis)) // not semantic blocking, not advisable

    def notify(signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- Concurrent[F].pure(s"[alarm] timer started...").debug
      _ <- signal.get
      _ <- Concurrent[F].pure("[alarm] Time's Up!").debug
    } yield ()

    def tickingClock(tickRef: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- unsafeSleep(1.second)
      tick <- tickRef.updateAndGet(_ + 1)
      _ <- Concurrent[F].pure(s"[clock] tick: $tick").debug
      _ <- if (tick >= 10) signal.complete(()).void else tickingClock(tickRef, signal)
    } yield ()

    for {
      tickRef <- Concurrent[F].ref(0)
      signal <- Concurrent[F].deferred[Unit]
      notifier <- notify(signal).start
      clock <- tickingClock(tickRef, signal).start
      _ <- notifier.join
      _ <- clock.join
    } yield ()
  }

  /*
  Exercises
  1. generalize the racePair definition
  2. generalize the Mutex concurrent primitive for any F
   */

  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]),
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B])
  ]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  def ourRacePair[F[_]: Concurrent, A, B](fa: F[A], fb: F[B]): F[RaceResult[F, A, B]] = Concurrent[F].uncancelable { poll =>
    for {
      signal <- Concurrent[F].deferred[EitherOutcome[F, A, B]]
      fibA <- fa.guaranteeCase(outA => signal.complete(Left(outA)).void).start
      fibB <- fb.guaranteeCase(outB => signal.complete(Right(outB)).void).start
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

  // generic mutex
  abstract class MyPolyMutex[F[_]] {
    def acquire: F[Unit]
    def release: F[Unit]
  }

  object MyPolyMutex {
    case class State[F[_]](locked: Boolean, waiting: Queue[Deferred[F, Unit]])

    def apply[F[_]: Concurrent]: F[MyPolyMutex[F]] = {
      val unlocked = State[F](locked = false, Queue())

      Concurrent[F].ref(unlocked).map { state =>
        new MyPolyMutex[F] {
          override def acquire: F[Unit] = Concurrent[F].uncancelable { poll =>

            def cleanup(signal: Deferred[F, Unit]) = state.modify {
              case State(lock, queue) => State[F](lock, queue.filterNot(_ eq signal)) -> release
            }.flatten

            for {
              signal <- Concurrent[F].deferred[Unit]
              _ <- state.modify {
                case State(false, _) => State[F](true, Queue()) -> Concurrent[F].unit
                case State(true, queue) => State[F](true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup(signal))
              }.flatten
            } yield ()
          }

          // already uncancellable
          override def release: F[Unit] =
            state.modify {
              case State(false, _) => unlocked -> Concurrent[F].unit
              case State(true, queue) if queue.isEmpty => unlocked -> Concurrent[F].unit
              case State(true, queue) => State(true, queue.tail) -> queue.head.complete(()).void
            }.flatten
        }
      }
    }
  }

  override def run: IO[Unit] = tenSecondAlarm[IO]
}
