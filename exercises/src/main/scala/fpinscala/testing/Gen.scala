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
      case f: Falsified => f
      case pp => p.run(max, n, rng)
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
      case Falsified(m, i) => Falsified(msg + "\n" + m, i)
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

  case object Proved extends Result {
    def isFalsified = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s" + "\n" +
      s"generated an exception: ${e.getMessage}" + "\n" +
      s"stack trace: ${e.getStackTrace.mkString("\n")}"
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

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    val s = weighted(
      choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
      unit(Executors.newCachedThreadPool) -> 0.25
    )
    forAll(s ** g) { case (s, a) => f(a)(s).get }
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def checkPar(p: Par[Boolean]): Prop = {
    forAllPar(unit())(_ => p)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: => RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n $msg ")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
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

  //exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => listOfN(n max 1, g))
  }

  def pint: Gen[Par[Int]] = choose(0, 10).map(Par.unit)

  //exercise 8.16
  def pint2: Gen[Par[Int]] = {
    choose(0, 100).listOfN(choose(0, 20)).map(l =>
      l.foldLeft(Par.unit(0))((p, i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))(_ + _)
        }))
  }
}

case class Gen[A](sample: State[RNG, A]) {

  def stateValue(seed: => Long = System.currentTimeMillis()): (A, RNG) = {
    this.sample.run(RNG.Simple(seed))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(sample.map2(g.sample)(f))
  }

  //exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  //exercise 8.10
  def unSized: SGen[A] = SGen((i: Int) => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = {
    this.map2(g)((_, _))
  }
}

case class SGen[A](forSize: Int => Gen[A]) {
  //exercise 8.11
  def map[B](f: A => B): SGen[B] = {
    SGen(forSize andThen (_.map(f)))
  }

  def flatMap[B](f: A => Gen[B]): SGen[B] = {
    SGen(forSize andThen (_.flatMap(f)))
  }
}

case object SampleProps {
  val smallIntGen = Gen.choose(-9, 10)
  val maxProp = forAll(listOf1(smallIntGen)) { ns =>
    val maxN = ns.max
    !ns.exists(_ > maxN)
  }

  // exercise 8.14
  val sortedProp = forAll(listOf1(smallIntGen))(ns => {
    val nsSorted = ns.sorted
    nsSorted.foldLeft(true, Int.MinValue) {
      case ((b, i1), i2) => (b && (i1 <= i2), i2)
    }._1 &&
      nsSorted.forall(ns.contains(_)) &&
      !nsSorted.exists(!ns.contains(_))
  })

  val p2: Prop = checkPar {
    Par.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  //exercise 8.17
  val forkProp: Prop = {
    forAllPar(pint2)(n => Par.equal(Par.fork(n), n))
  }
}
