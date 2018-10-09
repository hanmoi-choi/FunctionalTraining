package com.rea.higherorder

/**
  * DO NOT ATTEMPT these exercises until you've completed the recursion ones.
  *
  * foldLeft will reduce a list of A's down to a B. It takes an initial value of type B
  * and a list of A's.  It also takes a function which takes the accumulated value of type B
  * and the next value in the list (of type A) and returns a value which will be feed back into
  * the accumulator of the next call.
  *
  * As the name suggests it processes the list from left to right.
  *
  * Have a close look at your implementations from the RecursionExercises.  Which parts could you
  * pull out to a function to make them all common? Your implementation will be very close to
  * foldLeft.
  *
  * Good luck!
  *
  */

object FoldingExercises {

  def foldLeft[A, B](initialValue: B, list: List[A])(f: (B, A) => B): B = list match {
    case Nil => initialValue
    case h::tail => foldLeft[A, B](f(initialValue, h), tail)(f)
  }

  /**
   * foldRight is the same as foldLeft, except it processes the list from right to left.
   */
  def foldRight[A, B](initialValue: B, list: List[A])(f: (A, B) => B): B = list match {
    case Nil => initialValue
    case h::tail => f(h, foldRight[A, B](initialValue, tail)(f))
  }
  /**
   * Remember these, from our recursion exercises?  They can all be implemented with either
   * foldLeft or foldRight.
   */

  def sum(x: List[Int]): Int = foldLeft(0, x)(_+_)

  def length[A](x: List[A]): Int = foldLeft(0, x)((b, _) => b + 1)

  //Careful you'll need a type annotation on the initialValue field
  def map[A, B](x: List[A])(f: A => B): List[B] = foldRight(Nil: List[B], x)((a, b) => f(a)::b)

  def filter[A](x: List[A], f: A => Boolean): List[A] = foldRight(Nil: List[A], x)((a, b) => if (f(a)) a::b else b)

  def append[A](x: List[A], y: List[A]): List[A] = foldRight(y, x)(_::_)

  def flatten[A](x: List[List[A]]): List[A] = foldRight(Nil: List[A], x)(_:::_)

  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = foldRight(Nil: List[B], x)((a, b) => f(a):::b)

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = foldRight(0, x)((a, b) => if (a >= b) a else b)

  def reverse[A](x: List[A]): List[A] = foldRight(Nil: List[A], x)((a, b) => b :+ a)
}
