package chapter3

import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends List[Nothing]

case class cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else cons(as.head, apply(as.tail: _*))
  }

  //exercise 2
  def tail[A](list: List[A]): List[A] = {
    list match {
      case cons(_, tail) => tail
      case Nil => Nil
    }
  }

  //exercise 3
  def setHead[A](head: A, list: List[A]): List[A] = {
    cons(head, list)
  }

  //exercise 4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (list, 0) => list
      case (cons(_, x), num) if num > 0 => drop(x, num - 1)
    }
  }


  val xs: List[Int] = List(1, 2, 3, 4, 5)
  val ex1 = dropWhile(xs, (x: Int) => x < 4)

  //exercise 5 ???
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }
  }

  // my exercise
  def holdWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case cons(head, tail) if f(head) => cons(head, holdWhile(tail, f))
      case cons(head, tail) if !f(head) => holdWhile(tail, f)
    }
  }

  //example
//  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
//    l match {
//      case cons(h, t) if f(h) => dropWhile(t)(f)
//      case _ => l
//    }

  //surprisingly function
  def append[A](list1: List[A], list2: List[A]): List[A] = {
    list1 match {
      case Nil => list2
      case cons(head, tail) => cons(head, append(tail, list2))
    }
  }


  //exercise 6
  def init[A](l: List[A]): List[A] = {
    l match {
      case cons(head, cons(_, Nil)) => cons(head,Nil)
      case cons(head, tail) => cons(head, init(tail))
    }
  }

  //example : listing 3.3 page 49
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0)((x, y) => x + y)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)


}