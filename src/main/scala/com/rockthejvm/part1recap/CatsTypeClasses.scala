package com.rockthejvm.part1recap

object CatsTypeClasses {

  /*
  - applicative
  - functor
  - flatMap
  - monad
  - apply
  - applicativeError/monadError
  - traverse
  */

  // functor - "mappable" data structures
  trait MyFunctor[F[_]] {
    def map[A,B](initialValie: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  val listFunctor = Functor[List]

  // generalizable "mapping" APIs
  def icrement[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.*
  def increment_v2[F[_] : Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)

  // applicative - the ability to "lift" or "wrap" types
  // i.e. a constructor for the type F
  trait MyApplicative[F[_]] extends MyFunctor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList: List[Int] = applicativeList.pure(43)
  import cats.syntax.applicative.* // iomport the pure extension method
  val aSimpleList_v2: List[Int] = 43.pure[List]

  // flatMap - ability to chain multiple computations
  trait MyFlatMap[F[_]] extends MyFunctor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList = FlatMap[List]
  import cats.syntax.flatMap.* //flatMap extension method
  def crossProduct[F[_] : FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)


  // Monad - applicative + flatMap
  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A,B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  }

  import cats.Monad
  val monadList = Monad[List]
  def crossProduct_v2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  /*
    Type class hierarchy so far
      Functor -> FlatMap -->
              \             \
             Applicative --> Monad
  */

  // error-like type classes
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val applicativeErrorEither = ApplicativeError[ErrorOr, String]
  val desirableValue: ErrorOr[Int] = applicativeErrorEither.pure(42)
  val failedValue: ErrorOr[Int] = applicativeErrorEither.raiseError("Something failed")
  import cats.syntax.applicativeError.* // raiseError extension method
  val failedValue_v2: ErrorOr[Int] = "Something failed".raiseError[ErrorOr, Int]

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]
  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]


  // traverse
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  }

  // use case for traverse is to turn wrappers inside out
  // List[Option[Int]] -> Option[List[Int]]
  val listOfOptions: List[Option[Int]] = List(Some(1), Some(2), Some(3))
  import cats.Traverse
  val listTraverse = Traverse[List]
  val optionList: Option[List[Int]] = listTraverse.traverse(List(1,2,3))(x => Option(x))
  import cats.syntax.traverse.*
  val optionList_v2: Option[List[Int]] = List(1,2,3).traverse(Option(_))


  def main(args: Array[String]): Unit = {

  }
}
