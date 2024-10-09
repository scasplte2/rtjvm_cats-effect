package com.rockthejvm.part5polymorphic

import cats.Functor
import cats.effect.kernel.Outcome
import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import cats.effect.syntax.monadCancel._
import cats.implicits.toFunctorOps
import cats.syntax.flatMap._
import com.rockthejvm.utils.general._

import scala.concurrent.duration._

object PolyMorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  // MonadCancel
  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // we can create values
  val molIO: IO[Int] = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure("once started, I can't go back...")
      res <- monadCancelIO.pure(56)
    } yield res
  }


  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] =
    mc.uncancelable { _ =>
      for {
        _ <- mc.pure("once started, I can't go back...")
        res <- mc.pure(56)
      } yield res
    }

  val mustCompute_v2 = mustComputeGeneral[IO, Throwable]

  // allow cancelation listeners
  val mustComputeWithListener = mustCompute.onCancel(IO("I'm being canceled!").void)
  val mustComputeWithListener_v2 = monadCancelIO.onCancel(mustCompute, IO("I'm being canceled!").void)
  // .onCancel as extension method via cats.effect.syntax.monadCancel._

  // allow finalizers
  val aComputationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Outcome.Succeeded(fa) => fa.flatMap(a => IO(s"successful: $a").void)
    case Outcome.Errored(e) => IO(s"failed: $e").void
    case Outcome.Canceled() => IO("canceled").void
  }

  // bracket pattern is specific to MonadCancel
  val aComputationWithUsafge = monadCancelIO.bracket(IO(42)){ value =>
    IO(s"Using the meaning of life: $value")
  }{ _ =>
    IO(s"releasing the meaning of life...").void
  }

  /*
  Exercise - generalize a piece of code
   */
  def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis)) // not semantic blocking, not advisable

  def inputPassword[F[_], E](using mc: MonadCancel[F, E]): F[String] = for {
    _ <- mc.pure("input password:").debug
    _ <- mc.pure("typing password").debug
    _ <- unsafeSleep[F, E](2.seconds)
    pw <- mc.pure("_password")
  } yield pw

  def verifyPassword[F[_], E](pw: String)(using mc: MonadCancel[F, E]): F[Boolean] = for {
    _ <- mc.pure("verifying...").debug
    _ <- unsafeSleep[F, E](2.seconds)
    res <- mc.pure(pw == "_password")
  } yield res

  def authFlow[F[_], E](using mc: MonadCancel[F, E]): F[Unit] = mc.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(mc.pure("Authentication time out. Try again later.").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) mc.pure("Authentication successfull!").debug
           else mc.pure("Authentication failed.").debug
    } yield ()
  }

  def authProgram: IO[Unit] = for {
    authFib <- authFlow[IO, Throwable].start
    _ <- IO.sleep(3.seconds) >> IO("Auth timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run: IO[Unit] = authProgram
}
