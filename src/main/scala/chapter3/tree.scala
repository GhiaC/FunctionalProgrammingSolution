package chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //exercise 25
  def size[A](t: Tree[A]): Int = {
    def innerSize(t: Tree[A], acc: Int): Int = {
      t match {
        case Branch(_: Leaf[A], right: Tree[A]) => innerSize(right, acc + 1 + 1)
        case Branch(left: Tree[A], _: Leaf[A]) => innerSize(left, acc + 1 + 1)
        case Branch(_: Leaf[A], _: Leaf[A]) => acc + 1 + 2
        case Branch(left: Tree[A], right: Tree[A]) => innerSize(left, innerSize(right, acc) + acc + 1)
        case _ => acc
      }
    }

    innerSize(t, 0)
  }

  //exercise 26
  def maximum(t: Tree[Int]): Int = {
    def innerMax(t: Tree[Int], max: Int): Int = {
      t match {
        case Branch(leaf: Leaf[Int], right: Tree[Int]) => innerMax(right, leaf.value max max)
        case Branch(left: Tree[Int], leaf: Leaf[Int]) => innerMax(left, leaf.value max max)
        case Branch(leafL: Leaf[Int], leafR: Leaf[Int]) => leafL.value max leafR.value
        case Branch(left: Tree[Int], right: Tree[Int]) => innerMax(left, innerMax(right, max) max max)
        case _ => max
      }
    }

    innerMax(t, 0)
  }
}
