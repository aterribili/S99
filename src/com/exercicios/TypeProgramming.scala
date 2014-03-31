package com.exercicios

class TypeProgramming {

  trait Maybe[+T] {
    def map[B](f: T ==> B): Maybe[B]
    def filter(f: T ==> Boolean): Maybe[T]
    def getOrElse[B >: T](f: => B): B
  }

  trait ==>[-T, +B] {
    def apply(t: T): B
  }

  case class Some[+T](val value: T) extends Maybe[T] {

    def map[B](function: T ==> B) =
      Some(function(value))

    def filter(f: T ==> Boolean) =
      if (f(value)) this
      else None

    def getOrElse[B >: T](f: => B) =
      value

  }

  case object None extends Maybe[Nothing] {
    def map[B](f: Nothing ==> B) =
      None

    def filter(f: Nothing ==> Boolean) =
      None

    def getOrElse[T](f: => T) =
      f

  }

}
