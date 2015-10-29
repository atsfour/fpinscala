package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  //exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, j: Int) = i + j
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, j: Int) = i * j
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 || b2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 && b2
    val zero = true
  }

  //exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]) = o1.orElse(o2)
    val zero: Option[A] = None
  }

  //exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f.compose(g)
    val zero: A => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A) = m.op(y, x)
    val zero: A = m.zero
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._

  //exercise 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(gen ** gen ** gen){
      case ((a1, a2), a3) => m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
    } &&
    forAll(gen)(a => m.op(a, m.zero) == a) &&
    forAll(gen)(a => m.op(m.zero, a) == a)
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  //exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //exercise 10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
  }

  //exercise 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.length match {
    case 0 => m.zero
    case 1 => f(as.head)
    case n =>
      val (lHalf, rHalf) = as.splitAt(n / 2)
      m.op(foldMapV(lHalf, m)(f), foldMapV(rHalf, m)(f))
  }

  //exercise 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(p1: Par[A], p2: Par[A]): Par[A] = p1.map2(p2)(m.op)
    val zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] ={
    Par.parMap(v)(f).flatMap(bs => foldMapV(v, par(m))(Par.asyncF(f)))
  }

 //exercise 10.9
  def orderedWithoutMonoid(ints: IndexedSeq[Int]): Boolean = {
    ints.foldLeft((Int.MinValue, true)){case ((acc, b), i) => (i, b && (i >= acc))}._2
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    type OIIB = Option[(Int, Int, Boolean)]
    def orderdMonid: Monoid[OIIB] = new Monoid[OIIB] {
      def op(o1: OIIB, o2: OIIB): OIIB = (o1, o2) match{
        case (None, None) => None
        case (Some(x), None) => Some(x)
        case (None, Some(x)) => Some(x)
        case (Some((x1, y1, b1)), Some((x2, y2, b2))) => {
          Some((x1 min x2, y1 min y2, b1 && b2 && y1 <= x2))
        }
      }
      val zero = None
    }
    foldMapV(ints, orderdMonid)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, i, r)) => Part(a + l, i, r)
      case (Part(l, i, r), Stub(b)) => Part(l, i, r + b)
      case (Part(l1, i1, r1), Part(l2, i2, r2)) =>
        Part(l1, i1 + (if ((r1 + l2).isEmpty) 0 else 1) + i2, r2)
    }
    val zero = Stub("")
  }

  //exercise 10.11
  def count(s: String): Int = {
    def toWC(c: Char): WC = {
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    }
    def unstub(s: String): Int = if (s == "") 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(toWC) match {
      case Stub(s) => unstub(s)
      case Part(l, i, r) => unstub(l) + 1 + unstub(r)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

