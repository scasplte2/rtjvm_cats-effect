package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

import scala.concurrent.duration._

object AsyncIOs extends IOApp.Simple {

  import com.rockthejvm.utils.*

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread...")
    42
  }
  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try(computeMeaningOfLife()).toEither

  def computeMolOnThreadPool(): Unit = {
    threadPool.execute(() => computeMeaningOfLife())
  }

  // how to manage the return of a value from a thread that Cats-effect does not control?
  // Use Async to lift computation to an IO
  // async is a FFI (foreign function interface)
  val asyncMolIO: IO[Int] = IO.async_ { (cb: Callback[Int]) => // CE thread blocks (semantically) until this cb is invoked by some other thread
    threadPool.execute { () => // computation not managed by CE
      val result = computeMeaningOfLifeEither()
      cb(result) // CE thread is notified with the result
    }
  }

  def testAsyncMol_v1() = asyncMolIO.debug >> IO(threadPool.shutdown())

  /**
   * Exercise
   */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = IO.async_[A] { (cb: Callback[A]) =>
    ec.execute { () =>
      val result = Try(computation()).toEither
      cb(result)
    }
  }

  val asyncMol_v2: IO[Int] = asyncToIO(computeMeaningOfLife)(ec)

  def testAsyncMol_v2 = asyncMol_v2.debug >> IO(threadPool.shutdown())

  // 2
  lazy val molFuture: Future[Int] = Future { computeMeaningOfLife() }
  def futureToIO[A](future: => Future[A])(implicit ec: ExecutionContext): IO[A] =
    IO.async_ { cb =>
      future.onComplete { res =>
        cb(res.toEither)
      }
    }

  val asyncMolIO_v3: IO[Int] = futureToIO(molFuture)
  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture))

  // 3
  // define a never-ending IO
  val neverEndingIO: IO[Unit] = IO.async_ { _ => () }
  val neverEndingIO_v2: IO[Int] = IO.never[Int]

  // Full Async API
  // used async_ so far but the interface is more complicated
  // must account for cancellation
  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      // need to return IO[Option[IO[Unit]]]]
      // allows for a finalizer in case computation gets cancelled
      // not specifying the finalizer => Option[IO[Unit]]
      // creating option is an effect => IO[Option[IO[Unit]]]]
      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled!").debug.void))
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("cancelling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run = demoAsyncCancellation()

}
