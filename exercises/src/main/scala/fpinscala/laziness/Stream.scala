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

  //exercise 5.13
  def mapUnfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    unfold((this, n)){
      case (Cons(h, t), nn) if nn > 0 => Some((h(), (t(), nn - 1)))
      case _ => None
    }
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zip[B](s: Stream[B]): Stream[(A, B)] = {
    this.zipWith(s)((_, _))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)){
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (empty, t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }
  }

  //exercise 5.14
  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.isDefined).forAll(p => p._1 == p._2)
  }

  //exercise 5.15
  def tails: Stream[Stream[A]] = {
    unfold(this){
      case Cons(h, t) => Some((Cons(h, t), t()))
      case Empty => None
    } append empty
  }

  def hasSubsequence[B](s: Stream[B]): Boolean = {
    tails.exists(_.startsWith(s))
  }

  //exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))(
      (a, pair) => {
        lazy val p = pair
        val b = f(a, p._1)
        (b, p._2)
      }
    )._2
  }

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
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  //exercise 5.9
  def from(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = cons(n, from(n + 1))
    tail
  }

  //exercise 5.10
  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, go(n2, n1 + n2))
    }
    go(0, 1)
  }

  //exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, st)) => cons(a, unfold(st)(f))
    case None => empty
  }

  //exercise 5.12
  def constantUnfold[A](a: A): Stream[A] = {
    unfold(a)(s => Some((a, s)))
  }
  def fromUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))
  }
  def fibsUnfold: Stream[Int] = {
    unfold((0, 1)){case (n1: Int, n2: Int) => Some((n1, (n2, n1 + n2)))}
  }
}