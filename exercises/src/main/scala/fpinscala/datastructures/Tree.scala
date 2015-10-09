package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size1[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(l, r) => 1 + size1(l) + size1(r)
  }

  def maximum1(t: Tree[Int]): Int = {
    def go(t: Tree[Int], acc: Int) = t match {
      case Leaf(i) => i.max(acc)
      case Branch(l, r) => maximum1(l) max maximum1(r)
    }
    go(t, Int.MinValue)
  }

  def depth1[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 0
    case Branch(l, r) => 1 + (depth1(l) max depth1(r))
  }

  def map1[A,B](t: Tree[A])(f: A =>B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map1(l)(f), map1(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size[A](t: Tree[A]): Int = {
    fold(t)(x => 1)((x, y) => x + y + 1)
  }

  def maximum(t: Tree[Int]): Int = {
    fold(t)(x => x)((x, y) => x max y)
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)(x=> 0)((x, y) => 1 + (x max y))
  }

  def map[A,B](t: Tree[A])(f: A =>B): Tree[B] = {
    fold(t)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
  }

}