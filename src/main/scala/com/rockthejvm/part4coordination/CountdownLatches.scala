package com.rockthejvm.part4coordination

import cats.effect.kernel.Deferred
import cats.effect.std.CountDownLatch
import cats.effect.{IO, IOApp, Ref, Resource}
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.rockthejvm.utils.*

import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.concurrent.duration.*
import scala.io.Source


object CountdownLatches extends IOApp.Simple {

  def raceProgram(): IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    _ <- trigger(latch).start
    _ <- (1 to 10).toList.parTraverse(createRunner(_, latch))
  } yield ()
  
  /**
   * CDLatches are a coordination primitive initialized with a count.
   * All fibers calling await() on the CDLatch are (semantically) blocked.
   * When the internal count of the latch reaches 0 (via release() calls from other fibers), all waiting fibers are unblocked
   */

  def trigger(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO("5...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("4...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("3...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("2...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("1...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("GO GO GO!").debug
  } yield ()
  
  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[runner $id] waiting for signal").debug
    _ <- latch.await // block this fiber until the count reaches 0
    _ <- IO(s"[runner $id] RUNNING!").debug
  } yield ()

  override def run = downloadFile("chunks", "/home/ruthie/")
  
  /*
  - call the file server API and get the number of chunks (n)
  - start a CDLatch
  - start n fibers which download a chunk of the file (use the file server's download chunk API)
  - block on the latch until each task has finished
  - after all chunks are done, stitch the files together under the same file on disk
   */
  def downloadFile(filename: String, destination: String): IO[Unit] = for {
    numChunks <- FileServer.getNumChunks
    latch <- CountDownLatch[IO](numChunks)
    _ <- IO(s"Download started on $numChunks fibers.").debug
    _ <- (0 until numChunks).toList.parTraverse { idx =>
      for {
        chunk <- FileServer.getFileChunk(idx)
        partialName = destination + filename + idx
        _ <- IO(s"Downloading $partialName").debug
        _ <- FileServer.writeToFile(partialName, chunk)
        _ <- latch.release
        _ <- IO(s"Got $partialName").debug
      } yield ()
    }
    _ <- latch.await
    _ <- (0 until numChunks).toList.traverse { idx =>
      val partialName = destination + filename + idx
      FileServer.appendFileContents(partialName, destination + filename)
    }
  } yield ()
  
  /**
   * Exercise: simulate a file downloader on multiple threads
   */
  object FileServer {
    val fileChunksList = Array(
      "I love Scala.",
      "Cats Effect seems quite fun.",
      "Never would I have thought I would do low-level concurrency WITH pure FP!"
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)
    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))

    def writeToFile(path: String, contents: String): IO[Unit] = {
      val fileResources = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
      fileResources.use { writer =>
        IO(writer.write(contents))
      }
    }

    def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
      val compositeResource = for {
        reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
        writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
      } yield (reader, writer)

      compositeResource.use {
        case (reader, writer) => IO(reader.getLines().foreach(line => writer.write(line + "\n")))
          .guarantee(IO(Files.delete(Paths.get(fromPath))))
      }
    }
  }
}

// Exercise: implement your own CDLach with Ref and Deferred

abstract class CDLatch[F[_]] {
  def release: F[Unit]
  def await: F[Unit]
}

object CDLatch {

  // my attempt
  // problem - this could be reused
  def create_v1(count: Int): IO[CDLatch[IO]] = for {
    state <- Ref[IO].of(count)
    signal <- Deferred[IO, Unit]
    cdLatch = new CDLatch[IO] {
      override def release: IO[Unit] = state.modify { prevCount =>
        val newCount = prevCount - 1
        if (newCount > 0) newCount -> IO.unit
        else newCount -> signal.complete(())
      }

      override def await: IO[Unit] = signal.get
    }
  } yield cdLatch

  def create_v2(count: Int): IO[CDLatch[IO]] = for {
    signal <- Deferred[IO, Unit]
    state <- Ref[IO].of[State](Live(count))
  } yield new CDLatch[IO] {
    override def release: IO[Unit] = state.modify {
      case Done => Done -> IO.unit
      case Live(1) => Done -> signal.complete(()).void
      case Live(r) => Live(r - 1) -> IO.unit
    }.flatten

    override def await: IO[Unit] = state.get.flatMap {
      case Done => IO.unit
      case Live(_) => signal.get
    }
  }

  // Daniel's version
  sealed trait State

  case class Live(remaining: Int) extends State

  case object Done extends State
}

