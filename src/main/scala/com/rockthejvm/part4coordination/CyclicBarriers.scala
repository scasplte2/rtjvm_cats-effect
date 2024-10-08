package com.rockthejvm.part4coordination

import cats.Monad
import cats.effect.kernel.GenConcurrent
import cats.effect.std.CyclicBarrier
import cats.effect.{Deferred, IO, IOApp, Ref, Sync}
import cats.syntax.all.*
import com.rockthejvm.utils.*

import scala.collection.immutable.Queue
import scala.util.Random
import scala.concurrent.duration.*

object CyclicBarriers extends IOApp.Simple {

  /*
  A cyclic barrier is a coordination primitive that
  - is initialized with a count
  - has a single API: await

  A cyclic barrier will (semantically) block all fibers calling its await() method until there are exactly N fibers
  waiting, at which point the barrier will unblock all fibers and reset to its original state. Any further fibers will
  once more be blocked until there are enough fibers to unblock once more.
   */

  // example: signing up for a social network just about to be launched
  def createUser(id: Int, barrier: CBarrier[IO]): IO[Unit] = for {
    _ <- IO.sleep((Random.nextDouble * 1500).toInt.millis)
    _ <- IO(s"[user $id] Signing up for the waitlist...").debug
    _ <- IO.sleep((Random.nextDouble * 500).toInt.millis)
    _ <- IO(s"[user $id] Added to the waitlist").debug
    _ <- barrier.await // block the fiber until there are enough users
    _ <- IO(s"[user $id] Joined the network!").debug
  } yield  ()

  def openNetwork(): IO[Unit] = for {
    _ <- IO("[announcer] The social network will launch once there are 10 users!").debug
    barrier <- CBarrier(5)
    _ <- (1 to 11).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  override def run: IO[Unit] = openNetwork()
}

/*
Exercise: Implement your own CB with Ref + Deferred
 */

abstract class CBarrier[F[_]] {
  def await: F[Unit]
}

object CBarrier {
  case class State(remaining: Int, signal: Deferred[IO, Unit])

  def apply(count: Int): IO[CBarrier[IO]] = for {
    signal <- Deferred[IO, Unit]
    ref <- Ref[IO].of(State(count, signal))
  } yield new CBarrier[IO] {
    override def await: IO[Unit] = Deferred[IO, Unit].flatMap { newSignal =>
      ref.modify {
        case State(1, s) => State(count, newSignal) -> s.complete(()).void
        case State(n, s) => State(n - 1, s) -> s.get
      }.flatten
    }
  }
}