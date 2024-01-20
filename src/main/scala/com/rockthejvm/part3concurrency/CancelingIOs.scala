package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*

object CancelingIOs extends IOApp.Simple {

  import com.rockthejvm.utils.*

  /**
   * Cancelling IOs
   * - fib.cancel
   * - IO.race & other APIs
   * - manual cancellation
   */

  val chainOfIOs = IO("waiting").debug >> IO.canceled >> IO(42).debug

  // uncancellable IOs - prevents another fiber from cancelling
  // example: online store, payment processor
  // payment process must NOT be cancelled
  val specialPaymentSystem = (
    IO("Payment running, don't cancel me...").debug >>
    IO.sleep(1.second) >>
    IO("Payment completed.").debug
  ).onCancel(IO("Cancellation of DOOM!").debug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
  The uncancellable API is more complex and more general, it takes a function from Poll[IO] => IO
  In the example above, we aren't using the Poll instance. The poll oject can be used to mark sections
  within the returned effect which CAN BE CANCELLED
  */

  /**
   * Example: authentication service.
   * Has two parts
   * - input password, can be cancelled, because otherwise we might block indefinitely on user input
   * - verify password, CANNOT be cancelled once it is started
   */

  val inputPassword = IO("input password:").debug >> IO("typing password").debug >> IO.sleep(2.seconds) >> IO("_password")
  val verifyPassword = (pw: String) => IO("verifying...").debug >> IO.sleep(2.seconds) >> IO(pw == "_password")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication time out. Try again later.").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication successfull!").debug
           else IO("Authentication failed.").debug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Auth timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /**
   * Uncancellable calls are MASKS which suppress cancellation
   * Poll calls are "gaps" within the uncancellable effect where cancellation can be re-injected
   */

  /**
   * Exercises
   */
  // 1 - cancellation discords all cancel points EXCEPT for this defined via the injected poll
  val cancelBeforeMol = IO.canceled >> IO(42).debug
  val uncalleableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug)

  //2 - wrapping a prior IO.cancelable with poll definitions eliminates those previous poll cancellation points
  // using IO.uncancelable(poll => poll(authFlow)).start will pass prior poll definitions to remain cancelable
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(3.seconds) >> IO("Auth timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run = threeStepProgram()

  // 3 - in cancelled during an uncancelable region, then the cancel is handled by the next cancelable region
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debug >> IO.sleep(1.second) >> IO("first cancel done").debug) >>
      IO("uncancelable").debug >> IO.sleep(1.second) >> IO("uncancel done").debug >>
      poll(IO("second cancelable").debug >> IO.sleep(1.second) >> IO("second cancel done").debug)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(2500.millis) >> IO("CANCELING").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

}
