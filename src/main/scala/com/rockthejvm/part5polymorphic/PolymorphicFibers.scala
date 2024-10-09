package com.rockthejvm.part5polymorphic

import cats.effect.{Fiber, IO, IOApp, MonadCancel, Outcome, Spawn}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.syntax.spawn._

import com.rockthejvm.utils.general._
import scala.concurrent.duration._

object PolymorphicFibers extends IOApp.Simple {

  // Spawn = create fibers for any effect
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] // creates a fiber
    def never[A]: F[A] // forever-suspending effect
    def cede: F[Unit] // a "yield" effect

    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[ // generic race operation
      (Outcome[F, E, A],  Fiber[F, E, B]),
      (Fiber[F, E, A], Outcome[F, E, B])
    ]]
  }

  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

  val mol = IO(42)
  val fiber: IO[Fiber[IO, Throwable, Int]] = mol.start

  // inherits monad cancel -> pure, map/flatMap, raiseError, uncancelable, start/never/cede
  val spawnIO = Spawn[IO] // fetch the given/implicit Spawn[IO]

  def ioOnSomeThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- spawnIO.start(io) // io.start assumes the presence of Spawn[IO] in scope
    res <- fib.join
  } yield res

  // generalize
  def effectOnSomeThread[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = for {
    fib <- fa.start
    res <- fib.join
  } yield res

  val molOnFiber = ioOnSomeThread(mol)
  val molOnFiber_v2 = effectOnSomeThread(mol)

  /*
  Exercise - generalize the following
   */

  def simpleRace[F[_]: Spawn, A, B](fa: F[A], fb: F[B]): F[Either[A, B]] =
    Spawn[F].racePair(fa, fb)
      .flatMap {
        case Left((outA, fibB)) => outA match {
          case Outcome.Succeeded(fa) => fibB.cancel >> fa.map(a => Left(a))
          case Outcome.Errored(e) => fibB.cancel >> Spawn[F].raiseError(e)
          case Outcome.Canceled() => fibB.join.flatMap {
            case Outcome.Succeeded(fb) => fb.map(b => Right(b))
            case Outcome.Errored(e) => Spawn[F].raiseError(e)
            case Outcome.Canceled() => Spawn[F].raiseError(new RuntimeException("Both effects were canceled"))
          }
        }
        case Right((fibA, outB)) => outB match {
          case Outcome.Succeeded(fb) => fibA.cancel >> fb.map(b => Right(b))
          case Outcome.Errored(e) => fibA.cancel >> Spawn[F].raiseError(e)
          case Outcome.Canceled() => fibA.join.flatMap {
            case Outcome.Succeeded(fa) => fa.map(a => Left(a))
            case Outcome.Errored(e) => Spawn[F].raiseError(e)
            case Outcome.Canceled() => Spawn[F].raiseError(new RuntimeException("Both effects were canceled"))
          }
        }
      }

  val fast = IO.sleep(3.seconds) >> IO(42).debug
  val slow = IO.sleep(2.seconds) >> IO("Scala").debug

  val race = simpleRace(fast, slow)

  override def run: IO[Unit] = race.void

}
