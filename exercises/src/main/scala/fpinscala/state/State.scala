package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def nonNegativeInt0(rng: RNG): (Int, RNG) = rng.nextInt match {
    //maybe correct, but a bit dirty
    case (Int.MinValue, r) => (0, r)
    case (i, r) if i < 0 => (-i, r)
    case (i, r) => (i, r)
  }

  //exercise 6.2
  def double0(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1) ,r)
  }

  //exercise 6.5
  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)
  }

  //exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int)(r: RNG)(acc: List[Int]): (List[Int], RNG) = c match {
      case n if n > 0 => {
        val (ii, rr) = r.nextInt
        go(n - 1)(rr)(ii :: acc)
      }
      case _ => (acc, r)
    }
    go(count)(rng)(Nil)
  }

  //exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  //exercise 6.7
  def sequence0[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    @tailrec
    def go(as: List[Rand[A]])(acc: List[A])(r: RNG): (List[A], RNG) = as match {
      case Nil => (acc, r)
      case h :: t => {
        val (aa, rr) = h(r)
        go(t)(aa :: acc)(rr)
      }
    }
    go(fs)(Nil)(rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil: List[A]))((h, rs) => map2(h, rs)(_ :: _))
  }

  def ints1(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  // exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n + 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })
  }

  //exercise 6.9
  def map_new[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2_new[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

}

case class State[S,+A](run: S => (A, S)) {
  import fpinscala.state.State._
  //exercise 6.10
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, ss) = run(s)
      f(a).run(ss)
    })
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  // exercise 6.10
  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](states: List[State[S, A]]): State[S, List[A]] = {
    states.foldRight(unit[S, List[A]](Nil))((h, ss) => h.map2(ss)(_ :: _))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
