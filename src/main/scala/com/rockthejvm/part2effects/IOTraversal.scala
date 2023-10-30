package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List(
    "I quite like CE",
    "Scala is great",
    "looking forward to some awesome stuff"
  )

  // use traverse

  import cats.Traverse
  import cats.instances.list._

  val listTraverse: Traverse[List] = Traverse[List]

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }


  def traverseFutures(): Unit = {
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    singleFuture.foreach(println)
  }

  import com.rockthejvm.utils._

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal!
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /**
   * Exercises
   */
  // use traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = Traverse[List].traverse(listOfIOs)(identity)

  // generalize sequence
  def sequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = Traverse[F].traverse(listOfIOs)(identity)


  // use paralel traverse API
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listOfIOs.parTraverse(identity)

  // generalize sequence
  def parSequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = listOfIOs.parTraverse(identity)

  // existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  // parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from exercises
  val parallelSingleIO_v3 : IO[List[Int]] = ios.parSequence // extension method from the Parallel syntax package


  override def run: IO[Unit] =
//    singleIO.map(_.sum).debug.void
    parallelSingleIO_v3.map(_.sum).debug.void
}
