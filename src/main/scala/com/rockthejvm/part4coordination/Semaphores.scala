package com.rockthejvm.part4coordination

import cats.effect.{IO, IOApp}
import cats.effect.std.Semaphore
import cats.syntax.parallel._

import scala.concurrent.duration._
import com.rockthejvm.utils._

import scala.util.Random

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  // example: limiting the number of concurrent sessions on a server
  def doWorkWhilteLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquire
    // critical part
    _ <- IO(s"[session $id] logged in, working...").debug
    res <- doWorkWhilteLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    // end of critical section
    _ <- sem.release
  } yield res

  def demoSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- login(1, sem).start
    user2Fib <- login(2, sem).start
    user3Fib <- login(3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquireN(requiredPermits)
    // critical part
    _ <- IO(s"[session $id] logged in, working...").debug
    res <- doWorkWhilteLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    // end of critical section
    _ <- sem.releaseN(requiredPermits)
    } yield res

  def demoWeightedSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- weightedLogin(1, 1, sem).start
    user2Fib <- weightedLogin(2, 2, sem).start
    user3Fib <- weightedLogin(3, 3, sem).start // never starts since requires too many permits
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  /**
   * Exercise
   * 1. find out if there is something wrong with this code
   *    - expected each user to block the other but got concurrent access instead
   * 2. why
   *    - new semaphore is created for each thread
   * 3. fix it
   *    - acquire lock outside of parallel traversal
   */
  // Semaphore with one permit is a mutex
  val mutex: IO[Semaphore[IO]] = Semaphore[IO](1)
  val users = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session $id] waiting to log in...").debug
        _ <- sem.acquire
        // critical part
        _ <- IO(s"[session $id] logged in, working...").debug
        res <- doWorkWhilteLoggedIn()
        _ <- IO(s"[session $id] done: $res, logging out...").debug
        // end of critical section
        _ <- sem.release
      } yield res
    }
  }


  override def run = users.debug.void

}
