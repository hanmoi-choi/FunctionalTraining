package com.rea.recursion

// Taken from http://tmorris.net/posts/scala-exercises-for-beginners/index.html

/**
 * Ok here are the rules.
 *
 * You can't use any of the standard list functions, like `map`, `filter`, `flatMap`, `append`, `:::`, `:+`, etc.
 * 
 * But you can always use `::` to construct a new list by prepending an element to another list.
 *
 * You CAN and are encouraged to use the solutions from the exercises below to solve the harder
 * ones towards the end.
 *
 * Keep an eye out for repetition and similarities between your answers.
 *
 * REMEMBER: Follow the types, they almost always guide you to the solution.  If it compiles and looks a little
 * too simple, it's probably correct.  As Sherlock Holmes once said, "Each one is suggestive, together they are
 * most certainly conclusive."
 *
 * See if you can make your solution tail recursive, where possible.
 *
 */

object RecursionExercises {

  def plusOne(n: Int) = n + 1

  def minusOne(n: Int) = n - 1

  // Add two non-negative Integers together.  You are only allowed to use plusOne and minusOne above
  def add(a: Int, b: Int): Int = {
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else loop(n - 1, acc + 1)

    loop(b, a)
  }

  // You are not permitted to use any list functions such as map, flatMap, ++, flatten etc
  def sum(l: List[Int]): Int = {
    def loop(n: List[Int], acc: Int): Int = n match {
      case Nil => acc
      case h::tail => loop(tail, acc + h)
    }

    loop(l, 0)
  }

  //Again no list functions are permitted for the following
  def length[A](x: List[A]): Int = {
    def loop(n: List[A], acc: Int): Int = n match {
      case Nil => acc
      case _ :: tail => loop(tail, acc + 1)
    }

    loop(x, 0)
  }
  // Do you notice anything similar between sum and length? Hmm...

  // Mapping over a list.  You are given a List of type A and a function converting an A to a B
  // and you give back a list of type B.  No list functions allowed!
  def map[A, B](x: List[A], f: A => B): List[B] = {
    def loop(n: List[A], acc: List[B]): List[B] = n match {
      case Nil => acc
      case h :: tail => loop(tail, acc :+ f(h))
    }

    loop(x, Nil)
  }
  // Given a function from A => Boolean, return a list with only those item where the function returned true.
  def filter[A](x: List[A], f: A => Boolean): List[A] = {
    def loop(n: List[A], acc: List[A]): List[A] = n match {
      case Nil => acc
      case h :: tail => if (f(h)) loop(tail, acc :+ h) else loop(tail, acc)
    }

    loop(x, Nil)
  }

  // This pattern should be familiar by now... psst... look at add.
  def append[A](x: List[A], y: List[A]): List[A] = {
    def loop(n: List[A], acc: List[A]): List[A] = n match {
      case Nil => acc
      case h :: tail => loop(tail, acc :+ h)
    }

    loop(y, x)
  }

  // Flatten a list of lists to a single list.  Remember you can't use list.flatten.  Can you use a previous
  // solution to solve this one?
  def flatten[A](x: List[List[A]]): List[A] = {
    def loop(n: List[List[A]], acc: List[A]): List[A] = n match {
      case Nil => acc
      case h :: tail => loop(tail, acc:::h)
    }

    loop(x, Nil)
  }

  // Follow the types.  You've done a great job getting here. Follow the types.
  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = {
    def loop(n: List[A], acc: List[B]): List[B] = n match {
      case Nil => acc
      case h :: tail => loop(tail, acc:::f(h))
    }

    loop(x, Nil)
  }

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = {
    def loop(n: List[Int], acc: Int): Int = n match {
      case Nil => acc
      case h :: tail => if (h > acc) loop(tail, h) else loop(tail, acc)
    }

    if (x.isEmpty) 0 else loop(x, 0)
  }

  // Reverse a list
  def reverse[A](x: List[A]): List[A] = {
    def loop(n: List[A], acc: List[A]): List[A] = n match {
      case Nil => acc
      case h :: tail => loop(tail, h::acc)
    }

    loop(x, Nil)
  }
}
