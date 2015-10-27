package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+ _]] {
  self =>
  // so inner classes may call methods of trait

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = {
    ParserOps(f(a))
  }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def char(c: Char): Parser[Char] = {
    string(c.toString) map (_.charAt(0))
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _) or succeed(Nil: List[A])
  }

  //exercise 9.1
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _)
  }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  //exercise 9.1
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p1, p2).map { case (a, b) => f(a, b) }
  }

  //exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    def go(n: Int, p: Parser[A], acc: Parser[List[A]]): Parser[List[A]] = n match {
      case i if i > 0 => go(n - 1, p, p.map2(acc)(_ :: _))
      case _ => acc
    }
    go(n, p, succeed(Nil: List[A]))
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = {
      self.or(p, p2)
    }

    def or[B >: A](p2: Parser[B]): Parser[B] = {
      self.or(p, p2)
    }

    def slice: Parser[String] = self.slice(p)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      equal(p, p.map(a => a))(in)
    }

    def charLaw(in: Gen[Char]): Prop = {
      forAll(in)(c => run(char(c))(c.toString) == Right(c))
    }

    def stringLaw(in: Gen[String]): Prop = {
      forAll(in)(s => run(s)(s) == Right(s))
    }

    def orSpec: Prop = {
      check(run("abra" | "cadabra")("abra") == Right("abra")) &&
        check(run("abra" | "cadabra")("cadabra") == Right("cadabra"))
    }

    def listOfNSpec: Prop = {
      check(run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")) &&
        check(run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")) &&
        check(run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))
    }

    def succeedLaw[A](in: Gen[A]): Prop = {
      forAll(in)(a => run(succeed(a))("") == Right(a))
    }

    //exercise 9.2
    def productAssociativeLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop = {
      def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
      def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
      val left = (p1 ** p2) ** p3 map unbiasL
      val right = p1 ** (p2 ** p3) map unbiasR
      equal(left, right)(in)
    }

    def productMapLaw[A,B,C,D](p1: Parser[A], p2: Parser[B])(f: A => C, g: B => D)(in: Gen[String]): Prop = {
      val left = p1.map(f) ** p2.map(g)
      val right = (p1 ** p2).map{case (a, b) => (f(a), g(b))}
      equal(left, right)(in)
    }
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}