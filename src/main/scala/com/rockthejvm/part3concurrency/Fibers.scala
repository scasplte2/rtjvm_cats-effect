package com.rockthejvm.part3concurrency

import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}

import scala.concurrent.duration.*

object Fibers extends IOApp.Simple {

  val meaningOfLife: IO[Int] = IO.pure(42)
  val favLang: IO[String] = IO.pure("Scala")

  import com.rockthejvm.utils._

  def sameThreadIOs(): IO[Unit] = for {
    one <- meaningOfLife.debug
    two <- favLang.debug
  } yield ()

  // introduce the fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // nearly impossible to directly create a Fiber

  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs(): IO[Unit] = for {
    _ <- aFiber
    _ <- favLang.debug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits on the fiber to terminate
  } yield result
  /*
  IO[ResultType of fib.join]
  fib.join = Outcome[IO, Throwable, A]

  // Possible Outcomes
  - Success with an IO
  - Failure with an exception
  - cancelled

  Fibers allow cancellation of the computation
  * */

  val someIoOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResutFromAnotherThread = someIoOnAnotherThread.flatMap {
    case Outcome.Succeeded(effect) => effect
    case Outcome.Errored(_) => IO(0)
    case Outcome.Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("No number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("Starting").debug >> IO.sleep(1.second) >> IO("done").debug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being canceled").debug.void)

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("canceling").debug // running on calling thread
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /**
   * Exercises
   */

  // 1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = for {
    fib <- io.start
    outcome1 <- fib.join
    result <- outcome1 match
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e) => IO.raiseError(e)
      case Outcome.Canceled() => IO.raiseError(new RuntimeException("Thread execution cancelled"))
  } yield result

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {

    val result = for {
      fibA <- ioa.start
      fibB <- iob.start
      resultA <- fibA.join
      resultB <- fibB.join
    } yield (resultA, resultB)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => (for {
        a <- fa
        b <- fb
      } yield (a, b))

      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("Thread execution cancelled"))
    }
  }

  def testEx2() = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug
    tupleIOs(firstIO, secondIO).debug.void
  }

  // 3
  def timeout[A](io: IO[A], timeout: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- (IO.sleep(timeout) >> fib.cancel).start // careful - fibers can leak
      result <- fib.join
    } yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Outcome.Canceled() => IO.raiseError(new RuntimeException("Thread execution cancelled"))
    }
  }

  def testEx3(): IO[Unit] = {
    val aComputation = IO("Starting").debug >> IO.sleep(3.second) >> IO("done!") >> IO(42)
    timeout(aComputation, 2.seconds).debug.void
  }



  override def run: IO[Unit] = {
//    runOnSomeOtherThread(meaningOfLife) // IO[Succeeded[IO[42]]]
//      .debug.void

//    testCancel()
//      .debug.void

//      testEx2()
    testEx3()
  }
}
