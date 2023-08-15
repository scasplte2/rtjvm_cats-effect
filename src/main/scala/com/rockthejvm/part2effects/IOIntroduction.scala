package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.IORuntime

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg should not have side effects
  val aDelayedIO: IO[Int] = IO.delay[Int]({
    println("I'm producing an integer")
    1024
  })

  // this will cause the side-effect to happen at construction time
//  val shouldNotDoThis: IO[Int] = IO.pure({
//    println("I'm producing an integer")
//    1024
//  })

  val aDelayedIO_v2: IO[Int] = IO { // apply == delay
    println("I'm producing an integer")
    256
  }

  // Entire program will be a composition of IO or other Effects
  // map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap((mol => IO.delay(println(mol))))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningofLife = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)
  val smallProgram_v2: IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /**
   * Exercises
   */

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast_jaa[A,B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    res <- iob
  } yield res

  def sequenceTakeLast[A,B](ioa: IO[A], iob: IO[B]): IO[B] = ioa.flatMap(_ => iob)

  def sequenceTakeLast_v2[A,B](ioa: IO[A], iob: IO[B]): IO[B] = ioa *> iob // andThen

  def sequenceTakeLast_v3[A,B](ioa: IO[A], iob: IO[B]): IO[B] = ioa >> iob // call by name (lazily evaluated)

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst_jaa[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    res <- ioa
    _ <- iob
  } yield res

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa.flatMap(res => iob.map(_ => res))

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa <* iob

  // 3 - repeat an IO effect forever
  // hint: flatMap and recursion
  def forever[A](io: IO[A])(implicit ioRuntime: IORuntime): IO[A] =
    io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A])(implicit ioRuntime: IORuntime): IO[A] = io >> forever_v2(io)

  def forever_v3[A](io: IO[A])(implicit ioRuntime: IORuntime): IO[A] = io.foreverM

  // 4 - convert an IO to a different type
  // hint: map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  // 5 - discard value inside IO, return Unit
  def asUnit[A](io: IO[A]): IO[Unit] = convert(io, ())

  def asUnit_v2[A](io: IO[A]): IO[Unit] = io.as(()) // discouraged

  def asUnit_v3[A](io: IO[A]): IO[Unit] = io.void // discards value and returns unit, encouraged

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      lastNumber <- IO(n)
      prevSum <- sumIO(n - 1)
    } yield prevSum + lastNumber


  // 7 (hard) - write a fibonacci IO that does NOT crash on recursion
  // hints: use recursion and flatMap
  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO.defer(fibonacci(n - 1)) // same as .delay(...).flatten
      prev <- IO.defer((fibonacci(n - 2)))
    } yield last + prev


  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
//    println(smallProgram_v2.unsafeRunSync())

    println(sumIO(20000).unsafeRunSync())
//    println(sum(20000))

    (1 to 100).foreach(i => println(fibonacci(i).unsafeRunSync()))

  }

}
