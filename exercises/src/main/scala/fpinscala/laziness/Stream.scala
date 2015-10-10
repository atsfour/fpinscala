package fpinscala.laziness

import Stream._

import scala.annotation.tailrec


trait Stream[+A] {

  def apply(n: Int): A = this match {
    case Cons(h, t) if n == 0 => h()
    case Cons(h, t) if n > 0 => t().apply(n - 1)
    case _ => throw new IndexOutOfBoundsException
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //exercise 5.1
  def toList0: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList0
  }

  def toList: List[A] = {
    val buf = new scala.collection.mutable.ListBuffer[A]
    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => {
        buf += h()
        go(t())
      }
    }
    go(this)
  }

  //exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ if n == 0 => this
    case _ => empty
  }

  //exercise 5.3
  def takeWhile0(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  //exercise 5.5
  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else empty)
  }

  //exercise 5.4
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  //exercise 5.6
  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, bs) => cons(f(a), bs))
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((a, as) => cons(a, as))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, as) => if (p(a)) cons(a, as) else as)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((a, bs) => f(a) append bs)
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //exercise 5.8
  def constant0[A](n: A): Stream[A] = Stream.cons(n, constant0(n))
  def constant[A](a: A): Stream[A] = {
    lazy val tail = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}