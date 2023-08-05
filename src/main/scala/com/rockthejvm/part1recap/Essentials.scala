package com.rockthejvm.part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // values
  val aBoolean: Boolean = false

  // expressions - are EVALUATED to a value
  val anIfExpression = if(2 >3) "Bigger" else "Smaller"

  // instructions vs. expression
  // instructions are side-effecting => Unit
  val theUnit = println("Hello, Scala") // Unit = () -> "void" in other languages

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance model: extend at most one class but mixin zero or more traits
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch!")
  }

  // singleton
  object MySingleton //singleton pattern in one line

  // companions
  object Carnivore // companion object of the class Carnivore
  // static methods (class specific) behaviors go in Object definition
  // instance specific behaviors go in the Class definition

  // generics
  class MyList[A]

  // method notation
  val three = 1 + 2     // <-- infix notation
  val anotherThree = 1.+(2) // equivalent to

  // functional programming
  val incrementor: Int => Int = x => x + 1
  val incremented = incrementor(4) // 5

  // higher-order functions
  // map, flatMap, filter
  val processedList = List(1,2,3).map(incrementor) // List(2,3,4)
  val aLongerList = List(1,2,3).flatMap(x => List(x, x + 1)) // List91,2,2,3,3,4)

  // for-comprehensions
  val checkerboard = List(1,2,3).flatMap(n => List('a','b','c').map(c => (n,c))) // cartesian product of values
  val anotherCheckerboard = for {
    n <- List(1,2,3)
    c <- List('a','b','c')
  } yield (n,c) // an equivalent expression to the one above


  // options and try
  val anOption: Option[Int] = Option(/* something that might be null */ 3) // -> Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2) // if Some(x) then Some(2x) else None

  val anAttempt = Try(/* something that might throw */ 42) //Success || Fail
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10)

  // pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match
    case Some(value) => s"the option is not empty: $value"
    case None => "the option is empty"

  // Futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future { // evaluated on another thread
    // a bit of code
    42
  }

  // wait for completion (async)
  aFuture.onComplete({
    case Failure(exception) => println(s"The meaning of value failed: $exception")
    case Success(value) => println(s"The Async meaning of life is $value")
  })

  // map a Future
  val anotherFuture = aFuture.map(_ + 1)

  // partial functions
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 43
    case 8 => 56
    case 100 => 1024
  }

  // couple more advanced bits
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }


  def main(args: Array[String]): Unit = {

  }

}
