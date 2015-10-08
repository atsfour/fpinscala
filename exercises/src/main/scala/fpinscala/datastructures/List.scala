package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  //exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of Nil")
    case Cons(x, xs) => xs
  }

  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("head of Nil")
    case Cons(x, xs) => x
  }

  //exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("head of Nil")
    case Cons(x, xs) => Cons(h, xs)
  }

  //exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case x => drop(tail(l), x - 1)
  }

  //exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) xs
      else dropWhile(tail(l), f)
    }
  }

  //exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of Nil")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = {
    def go(l: List[A])(count: Int): Int = l match {
      case Nil => count
      case Cons(x, xs) => go(xs)(count + 1)
    }
    go(l)(0)
  }

  //exercise 3.9
  def length2[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => y + 1)
  }

  //exercise 3.10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    @tailrec //stack-safe
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(x, xs) => go(xs, Cons(x, acc))
    }
    go(l, Nil)
  }

  def reverse2[A](l: List[A]): List[A] = {
    // implemented by foldRight, not stack-safe
    foldRight(l, Nil: List[A])((a, acc) => Cons(a, acc))
  }

  //exercise 3.13
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    // Exercise 3.13 Using foldLeft
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }

  //exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight2(a1, a2)((a, as) => Cons(a, as))
  }

  //exercise 3.15
  def appendAll[A](ls: List[List[A]]): List[A] = {
    foldRight2(ls, Nil: List[A])(append)
  }

  //exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight2(l, Nil: List[B])((a, l) => Cons(f(a), l))
  }

  //exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight2(l, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)
  }

  //exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    appendAll(map(as)(f))
  }

  //exercise 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  //exercise 3.23
  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    // if l1 and l2 are not same length, cut off the rest of longer one.
    l1 match {
      case Nil => Nil
      case Cons(x, xs) => l2 match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
      }
    }
  }

  //exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(l: List[A], prefix: List[A]): Boolean = (l, prefix) match{
      case (_, Nil) => true
      case (Cons(h, t), Cons(ph, pt)) if h == ph => startsWith(t, pt)
      case _ => false
    }
    sup match {
      case Nil => false
      case _ if startsWith(sup, sub) => true
      case Cons(x, xs) => hasSubsequence(xs, sub)
    }
  }
}