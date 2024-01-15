package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.{IO, IOApp, Resource}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.*

object Resources extends IOApp.Simple {

  import com.rockthejvm.utils._

  // why do we want Resources?
  // use-case: manage a connection lifecycle (manage a replaceable resource)
  class Connection(url: String) {
    def open(): IO[String] = IO(s"opening connection to $url").debug
    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  val asyncFetchurl = for {
    fib <- (new Connection("rockthejvm.com").open() >> IO.sleep(Int.MaxValue.seconds)).start
    _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield ()
  // probelm: the above leaks resources, this is a common issue with effectful operations


  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib <- (conn.open() >> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close().void).start
    _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield ()
  //^^ proper way to handle cancellation but is cumbersome so use bracket pattern isntead

  // bracket is the description of the resource acquisition and release so safe to spawn a new instance
  // without worrying about having to close each instance
  // another way to view
  // bracket pattern: someIO.bracket(useResourceCallback)(releaseResourceCallback)
  // this is equivalent to try-catches but in a pure FP way
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() >> IO.sleep(Int.MaxValue.seconds))(conn => conn.close().void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield ()

  /**
   * Exercise: read a file with the bracket pattern
   * - open a scanner
   * - read the file line by line, every 100 millis
   * - close the scanner
   * - if cancelled/throws error, close the scanner
   */
  def openFileScanner(path: String): IO[Scanner] = IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] = {
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit
  }

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"Opening scanner for $path") >>
      openFileScanner(path).bracket(readLineByLine){ scanner =>
        IO(s"closing file at $path").debug >> IO(scanner.close())
      }


  /**
   * Resources
   */
  def connectionFromConfig(path: String): IO[Unit] = {
    openFileScanner(path).bracket { scanner =>
      // acquire a connection based no the file
      IO(new Connection((scanner.nextLine()))).bracket { conn =>
        conn.open() >> IO.never
      }(conn => conn.close().void)
    }(scanner => IO("closing file").debug >> IO(scanner.close()))
  }

  // handling nested resources with the bracket pattern can be tedious
  // the resource lets us separate the acquisition and closing definition from its usage
  val connectionResource = Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close().void)
  // ... at a later part of the code (call time)
  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // resource are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string).debug")
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").debug.void

  // equivalent definitions
  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)

  // take the exercise solution from before and refactor to use Resource
   def fileScannerResource(path: String) = Resource.make(openFileScanner(path))(s => IO(s.close()).void).use(readLineByLine)

  def cancelReadFile(path: String) = for {
    fib <- fileScannerResource(path).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  // nested Resources
  def connectionFromConfigResource(path: String) =
    Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void))

  val openConnection = connectionFromConfigResource("src/main/resources/connection.txt").use(conn => conn.open() >> IO.never)
  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(1.second) >> IO("canceling!").debug >> fib.cancel
  } yield ()

  def connFromConfResourceClean(path: String) = for {
    scanner <- Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
    connection <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void)
  } yield connection

  // connection + file will close automatically

  // finalziers to regular IOs
  val ioWithFinalizer = IO("some resource").debug.guarantee(IO("freeing resource").debug.void)
  val ioWithFinalizer_v2 = IO("some resource").debug.guaranteeCase {
    case Outcome.Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result").debug).void
    case Outcome.Errored(e) => IO("nothing to release").debug.void
    case Outcome.Canceled() => IO("resource got canceled, releasing what is left").debug.void
  }

  override def run = connFromConfResourceClean("src/main/resources/connection.txt").use(c => c.open() >> IO.never)
    //canceledConnection
    //cancelReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
    //resourceFetchUrl.void
    //bracketReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")

}
