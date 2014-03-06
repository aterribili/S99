package com.exercicios

class p07 {
  println(flatten(List((1 :: 2 :: 3 :: 6 :: Nil), (1 :: 2 :: 3 :: 5 :: Nil))))

  def flatten(list: List[List[Int]]): List[Int] = {
    val newList = List[Int]();

    if (list.length == 0) newList;

    newList ::: list(0)
    list.drop(0)

    newList ::: flatten(list)
    newList
  }
}