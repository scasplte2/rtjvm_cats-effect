package com.rockthejvm.part4coordination

import cats.effect.kernel.Outcome
import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.syntax.parallel.*
import com.rockthejvm.utils.*

import scala.collection.immutable.Queue
import scala.concurrent.duration.*
import scala.util.Random

abstract class Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex {

  type Signal = Deferred[IO, Unit]
  val unlocked = State(locked = false, Queue())

  // my attempt
  def create_v1: IO[Mutex] = for {
    lock <- Ref[IO].of(false)
    signal <- Deferred[IO, Unit]
    mutex = new Mutex:
      override def acquire = for {
        status <- lock.get
        _ <- if (status) signal.get.void else lock.set(true)
      } yield ()

      override def release = for {
        status <- lock.get
        _ <- if (status) signal.complete(()) else lock.set(false)
      } yield ()
  } yield mutex

  def create_v2: IO[Mutex] = for {
    ref <- Ref[IO].of(Mutex.unlocked)
    mutex = Mutex.simpleMutex(ref)
  } yield mutex

  def create_v3: IO[Mutex] = for {
    ref <- Ref[IO].of(Mutex.unlocked)
    mutex = Mutex.cancellableMutex(ref)
  } yield mutex

  // mutex that does not support cancellation
  private def simpleMutex(state: Ref[IO, State]): Mutex = {
    new Mutex {
      override def acquire: IO[Unit] = for {
        signal <- Deferred[IO, Unit]
        _ <- state.modify {
          case State(false, _) => State(true, Queue()) -> IO.unit
          case State(true, queue) => State(true, queue.enqueue(signal)) -> signal.get
        }.flatten // modify returns IO[B], our B is IO[Unit] so modify returns IO[IO[Unit]] so we need to flatten
      } yield ()

      override def release: IO[Unit] = state.modify {
        case State(false, _) => unlocked -> IO.unit
        case State(true, queue) if queue.isEmpty => unlocked -> IO.unit
        case State(true, queue) => State(true, queue.tail) -> queue.head.complete(()).void
      }.flatten
    }
  }

  private def cancellableMutex(state: Ref[IO, State]): Mutex = {
    new Mutex {
      override def acquire: IO[Unit] = IO.uncancelable { poll =>

        def cleanup(signal: Signal) = state.modify {
          case State(lock, queue) => State(lock, queue.filterNot( _ eq signal)) -> release
        }.flatten

        for {
          signal <- Deferred[IO, Unit]
          _ <- state.modify {
            case State(false, _) => State(true, Queue()) -> IO.unit
            case State(true, queue) => State(true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup(signal))
          }.flatten
        } yield ()
      }

      // already uncancellable
      override def release: IO[Unit] =
        state.modify {
          case State(false, _) => unlocked -> IO.unit
          case State(true, queue) if queue.isEmpty => unlocked -> IO.unit
          case State(true, queue) => State(true, queue.tail) -> queue.head.complete(()).void
        }.flatten
      }
  }

  case class State(locked: Boolean, waiting: Queue[Signal])
}

  object MutexPlayground extends IOApp.Simple {
    def demoNonLockingTasks(): IO[List[Int]] =
      (1 to 10)
        .toList
        .parTraverse(createNonLockingTask)

    def createNonLockingTask(id: Int): IO[Int] = for {
      _ <- IO(s"[task $id] working...").debug
      res <- criticalTask()
      _ <- IO(s"[task $id] got result: $res").debug
    } yield res

    def criticalTask(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

    def demoSimpleMutex(): IO[List[Int]] = for {
      mutex <- Mutex.create_v2
      results <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
    } yield results

    def demoLockingTasks(): IO[List[Int]] = for {
      mutex <- Mutex.create_v1
      results <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
    } yield results

    def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
      _ <- IO(s"[task $id] waiting for permissions...").debug
      _ <- mutex.acquire // blocks if the mutex has been acquired by some other fiber
      // start critical section
      _ <- IO(s"[task $id] working...").debug
      res <- criticalTask()
      _ <- IO(s"[task $id] got result: $res").debug
      // end critical section
      _ <- mutex.release
      _ <- IO(s"[task $id] locked removed").debug
    } yield res

    def createCancellingTask(id: Int, mutex: Mutex): IO[Int] =
      if (id % 2 == 0) createLockingTask(id, mutex)
      else for {
        fib <- createLockingTask(id, mutex).onCancel(IO(s"[task $id] received cancellation!").debug.void).start
        _ <- IO.sleep(2.seconds) >> fib.cancel
        out <- fib.join
        result <- out match {
          case Outcome.Succeeded(fa) => fa
          case Outcome.Errored(_) => IO(-1)
          case Outcome.Canceled() => IO(-2)
        }
      } yield result

    def demoCancellingTasks(): IO[List[Int]] = for {
      mutex <- Mutex.create_v3
      results <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
    } yield results

    override def run = demoCancellingTasks().debug.void
  }