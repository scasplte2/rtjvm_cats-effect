package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

object BlockingIOs extends IOApp.Simple {

  import com.rockthejvm.utils._

  val someSleeps = for {
    _ <- IO.sleep(1.second).debug // yields control of the thread during sleep
    _ <- IO.sleep(1.second).debug // using SEMANTIC BLOCKING
  } yield ()

  // really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  } // will evaluate on a thread from ANOTHER thread pool specific for blocking calls

  // can control the way yielding a thread is managed
  val iosOnManyThreads = for {
    _ <- IO("first").debug
    _ <- IO.cede // a signal to yield control over the thread - equivalent to IO.shift
    _ <- IO("second").debug // the rest of this effect may run on another thread (not necessarily)
    _ <- IO.cede
    _ <- IO("third").debug
    _ <- IO.cede
  } yield ()

  def testThousandEffectSwitch() = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
  }

  override def run = testThousandEffectSwitch().void
}
