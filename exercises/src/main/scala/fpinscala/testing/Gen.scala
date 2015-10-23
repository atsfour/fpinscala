package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

  //exercise 8.8
  def double: Gen[Double] = {
    Gen(State(RNG.double))
  }
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val provabilityA = g1._2 / (g1._2 + g2._2)
    double.flatMap(d => if (d < provabilityA) g1._1 else g2._1)
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def map[A,B](f: A => B): Gen[B] = ???

  //exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

}

trait SGen[+A] {

}

