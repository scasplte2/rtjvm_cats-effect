package com.rockthejvm.part2effects

import scala.concurrent.Future

object Effects {

  // pure functional programming
  // substitution (referential transparency)
  //   is needed in order for a program to be a single expression to be evaluate
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // referential transparency = can replace an expression with its value
  //     as many times as we want without changing behavior

  // example: print to the console
  //val printSomething: Unit = println("Cats effect")
  val printSomething_v2: Unit = () // not the same as program with printSomething

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = anInt += 1
  val changingVar_v2: Unit = () // again lose referential transparency since this isn't the same

  // side-effects are inevitable for any useful program
  // effect is the wrapper of a side-effect into the application domain
  // Desires
  // - type signature describes the kind of calculation that will be performed
  // - type signature describes the VALUE that will be calculated
  // - when side-effects are needed, effect construction is separate from effect execution

  // example: Option
  // - describes a possibly absent value
  // - computes a value of type A, if it exists
  // - side effects are not needed
  // --- according to the above definition, Option is an effect type
  val anOption: Option[Int] = Option(42)

  /**
   * what about Future?
   * - describes an asynchronous computation that will perform in the future
   * - computes a value of type A, if it is successful
   * - side effect is required (allocating/scheduling a thread) -> execution is NOT separate from construction
   * --- Future is not a good effect type
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /**
   * MyIO data type from the Monads lesson
   * - describes a computation that might produce side-effects
   * - calculates a value of type A, if it is successful
   * - side-effects are required for the evaluation of () => A
   * --- So MyIO is an effect data type since the construction is separate from the side-effect action
   */

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun())).unsafeRun()
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something")
    42
  })

  /**
   * Exercises - Create the following:
   * 1. An IO which returns the current time of the system
   * 2. An IO which measures the duration of a computation
   *   (hint - use exercise number 1)
   * 3. An IO which prints something to the console
   * 4. An IO which reads a line (a string) from the std input
   */

  // 1
  val systemTimeIO: MyIO[Long] = MyIO(() => {
    System.currentTimeMillis()
  })

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    t1 <- systemTimeIO
    _ <- computation
    t2 <- systemTimeIO
  } yield (t2 - t1)

  /*
   deconstruct the for-comprehension
  systemTimeIO.flatMap(t1 => computation.flatMap(_ => systemTimeIO.map(t2 => (t2 - t1))))

  systemTimeIO.map(t2 => (t2 - t1)) = MyIO(() => systemTimeIO.unsafeRun() - t1)
  systemTimeIO.map(t2 => (t2 - t1)) = MyIO(() => System.currentTimeMillis() - t1)

  systemTimeIO.flatMap(t1 => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - t1)))

  computation.flatMap(_lambda) = MyIO(() => _lambda(___COMP___).unsafeRun())
                               = MyIO(() => MyIO(() => System.currentTimeMillis() - t1)).unsafeRun())
                               = MyIO(() => System.currentTimeMillis()_after_computation - t1)

  systemTimeIO.flatMap(t1 => MyIO(() => System.currentTimeMillis()_after_computation - t1))
  MyIO(() => (t1 => MyIO(() => System.currentTimeMillis()_after_computation - t1))(systemTimeIO.unsafeRun())
  MyIO(() => MyIO(() => System.currentTimeMillis_after_computation - System.currentTimeMillis_before).unsafeRun())
  MyIO(() => System.currentTimeMillis_after - System.currentTimeMillis_before)

  */

  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }

  // 3
  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  // 4
  val read: MyIO[String] = MyIO { () => scala.io.StdIn.readLine() }

  def testConsole(): Unit = {
    val program: MyIO[Unit] = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + line2)
    } yield ()

    program.unsafeRun()
  }

  def main(args: Array[String]): Unit = {
    //anIO.unsafeRun()


//    val longIO = MyIO(() => {
//      (1 to 100000).zipWithIndex.reverse.mkString(" ")
//    })
//
//    val measureResult = measure(longIO).unsafeRun()
//    println(measureResult)

//    val runInputIO = inputIO.unsafeRun()
//    println(runInputIO)

    //testTimeIO()

    testConsole()
  }

}
