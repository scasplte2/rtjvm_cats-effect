package com.rockthejvm.utils

import cats.Functor
import cats.implicits.toFunctorOps
import cats.effect.{IO, MonadCancel}

import scala.concurrent.duration.FiniteDuration

extension [A](io: IO[A])
  def debug: IO[A] = for {
    a <- io
    t = Thread.currentThread().getName
    _ = println(s"[$t] $a")
  } yield a

object general {

  extension [F[_] : Functor, A](fa: F[A]) {
    def debug: F[A] = fa.map { a =>
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    }
  }
}