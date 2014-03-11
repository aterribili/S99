package com.exercicios

object exercicios extends App {

  println(last(1 :: 2 :: 3 :: 5 :: Nil))
  println(penultimate(1 :: 2 :: 3 :: 5 :: Nil))
  println(nth(2, 1 :: 2 :: 3 :: 5 :: Nil))
  println(length(1 :: 2 :: 3 :: 4 :: Nil))
  println(reverse(1 :: 2 :: 3 :: 4 :: 5 :: Nil))
  println(isPalindrome(1 :: 2 :: 3 :: 2 :: 1 :: Nil))
  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  def last(list: List[Int]): Int =
    if (list.tail == Nil) list.head

    else last(list.tail)

  def penultimate(list: List[Int]): Int =
    if (list.tail.tail == Nil) list.head

    else penultimate(list.tail)

  def nth(k: Int, list: List[Int]): Int =
    if (k == 0) list.head

    else nth(k - 1, list.tail)

  def length(list: List[Int]): Int =
    if (list.tail == Nil) 1

    else 1 + length(list.tail)

  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => list
    case (x :: xs) => reverse(xs) ++ List(x)
  }

  def isPalindrome[A](list: List[A]) =
    list == reverse(list)

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  def compress(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case head :: tail => head :: compress(tail.dropWhile(_ == head))
  }

  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case head :: tail => List(head :: tail.takeWhile(_ == head)) ++ pack(tail.dropWhile(_ == head))
  }

  def encode[T](list: List[T]): List[(Int, T)] =
    pack(list).map { element => (element.length, element.head) }

  def encodeModified[T](list: List[T]): List[Any] = {
    encode(list) map { case (numero, elemento) => if (numero == 1) elemento else (numero, elemento) }
  }

}

