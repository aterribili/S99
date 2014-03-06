package com.exercicios

object p01 extends App {

  println(last(1 :: 2 :: 3 :: 5 :: Nil))
  println(penultimate(1 :: 2 :: 3 :: 5 :: Nil))
  println(nth(2, 1 :: 2 :: 3 :: 5 :: Nil))
  println(length(1 :: 2 :: 3 :: 4 :: Nil))
  println(reverse(1 :: 2 :: 3 :: 4 :: 5 :: Nil))

  def last(list: List[Int]): Int =
    {
      if (list.tail == Nil) list.head

      else last(list.tail)
    }

  def penultimate(list: List[Int]): Int =
    {
      if (list.tail.tail == Nil) list.head

      else penultimate(list.tail)
    }

  def nth(k: Int, list: List[Int]): Int = {
    if (k == 0) list.head

    else nth(k - 1, list.tail)
  }

  def length(list: List[Int]): Int = {
    if (list.tail == Nil) 1

    else 1 + length(list.tail)
  }

  def reverse[A](list: List[A]): List[A] = {
    list match {
      case Nil => list
      case (x :: xs) => reverse(xs) ::: List(x)
    }
  }

  println(flatten(List((1 :: 2 :: 3 :: 6 :: Nil), (1 :: 2 :: 3 :: 5 :: Nil))))

  def flatten(list: List[A]): List[A] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }


}

