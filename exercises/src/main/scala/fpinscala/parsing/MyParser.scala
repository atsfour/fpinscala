package fpinscala.parsing

import scala.util.matching.Regex
import MyParserTypes._

/**
 * Created by atsfour on 2015/10/28.
 */
object MyParserTypes {

  trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, b) => Failure(f(e), b)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommited: Boolean): Result[A] = this match {
      case Failure(e, b) => Failure(e, b || isCommited)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, i) => Success(a, i + n)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  case class ParseState(loc: Location) {
    def input: String = loc.input.substring(loc.offset)

    def slice(n: Int): String = loc.input.substring(loc.offset, loc.offset + n)

    def advancedBy(n: Int): ParseState =
      copy(loc = loc.advanceBy(n))
  }

  type Parser[+A] = ParseState => Result[A]

}

object MyParsers extends Parsers[Parser] {

  private def firstNomatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }

  //exercise 9.14
  def string(str: String): Parser[String] = {
    val msg = "'" + str + "'"
    s => {
      val i = firstNomatchingIndex(s.loc.input, str, s.loc.offset)
      if (i == -1)
        Success(str, str.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), i != 0)
    }
  }

  //exercise 9.13
  def regex(r: Regex): Parser[String] = {
    val msg: String = "regex " + r
    s => {
      r.findPrefixOf(s.input) match {
        case None => Failure(s.loc.toError(msg), isCommitted = false)
        case Some(m) => Success(m, m.length)
      }
    }
  }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = {
    s => {
      val res1 = p1(s)
      res1 match {
        case Failure(_, false) => p2(s)
        case r => r
      }
    }
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    val initState = ParseState(Location(input))
    p(initState).extract
  }

  //exercise 9.13
  def slice[A](p: Parser[A]): Parser[String] = s => p(s) match {
    case Success(_, n) => Success(s.slice(n), n)
    case f@Failure(_, _) => f
  }

  //exercise 9.13
  def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  def fail[A](msg: String): Parser[A] = s => Failure(s.loc.toError(msg), isCommitted = false)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
    s => p(s) match {
      case Success(a, n) => f(a)(s.advancedBy(n)).addCommit(n != 0).advanceSuccess(n)
      case ff@Failure(_, _) => ff
    }
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = {
    s => p(s).mapError(_.label(msg))
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = {
    s => p(s).mapError(_.push(s.loc, msg))
  }

  def attempt[A](p: Parser[A]): Parser[A] = {
    s => p(s).uncommit
  }


}
