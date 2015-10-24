package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  //exercise 8.9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case f => f
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case pass => pass
    }
  }

  def tag(msg: FailedCase): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(m, i) => Falsified(msg + "¥n" + m, i)
      case p => p
    }
  }
}

object Prop {

  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
  }


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
    forAll(i => g.forSize(i))(f)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesParSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesParSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: ${s}¥n" +
      s"generated an exception: ${e.getMessage}¥n" +
      s"stack trace: ${e.getStackTrace.mkString("¥n")}"
  }

}

object Gen {

  //Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))
  }

  //exercise 8.5
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.int).map(_ < 0))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def pair[A](g: Gen[A]): (Gen[A], Gen[A]) = {
    (g, Gen(g.sample.flatMap(a => State.unit(a))))
  }

  def option[A](g: Gen[A]): Gen[Option[A]] = {
    Gen(g.sample.map(Some(_)))
  }

  def char: Gen[Char] = {
    Gen(State(RNG.int).map(_.toChar))
  }

  def string(minLength: Int, maxLength: Int): Gen[String] = {
    val length = choose(minLength, maxLength + 1)
    Gen(length.sample.flatMap(listOfN(_, char).sample.map(_.mkString(""))))
  }

  //exercise 8.7 (using result of 8.8)
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    weighted((g1, 1.0), (g2, 1.0))
  }

  //exercise 8.8
  def double: Gen[Double] = {
    Gen(State(RNG.double))
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val provabilityA = g1._2 / (g1._2 + g2._2)
    double.flatMap(d => if (d < provabilityA) g1._1 else g2._1)
  }

  //exercise 8.11
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => listOfN(n, g))
  }
}

case class Gen[A](sample: State[RNG, A]) {

  def stateValue(seed: Int): (A, RNG) = {
    this.sample.run(RNG.Simple(seed))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(a => f(a)))
  }

  //exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  //exercise 8.10
  def unsized: SGen[A] = SGen((i: Int) => this)

}

case class SGen[+A](forSize: Int => Gen[A]) {
  //exercise 8.11
  def map[B](f: A => B): SGen[B] = {
    SGen(forSize andThen (_.map(f)))
  }

  def flatMap[B](f: A => Gen[B]): SGen[B] = {
    SGen(forSize andThen (_.flatMap(f)))
  }
}

