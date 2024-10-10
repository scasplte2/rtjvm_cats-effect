package com.rockthejvm.part5polymorphic

import cats.effect.{Concurrent, IO, IOApp, Temporal}
import cats.syntax.all._

import scala.concurrent.duration._
import com.rockthejvm.utils._

object PolymorphicTemporal extends IOApp.Simple {

  // Temporal - time-blocking effects

  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit] // semantically blocks fiber for a specified time
  }

  // abilities: pure, map/flatMap, raiseError, uncancelable, start, ref/deferred, +sleep
  val temporalIO = Temporal[IO]
  val chainOfEffects = IO("Loading...").debug *> IO.sleep(1.second) *> IO("Ready!").debug
  val chainOfEffect_v2 = temporalIO.pure("Loading...").debug *> temporalIO.sleep(1.second) *> temporalIO.pure("Ready!").debug

  /*
  Exercise - generalize the following
   */
  def timeout[F[_]: Temporal, A](fa: F[A], duration: FiniteDuration): F[A] =
    Temporal[F].race(Temporal[F].sleep(duration), fa)
      .flatMap {
        case Left(_) => Temporal[F].raiseError(new RuntimeException("Computation timed out"))
        case Right(value) => Temporal[F].pure(value)
      }

  override def run: IO[Unit] = ???
}
