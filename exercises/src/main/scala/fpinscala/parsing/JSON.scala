package fpinscala.parsing

import fpinscala.testing._
import fpinscala.testing.Prop._

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def spaces: Parser[String] = "\\s*".r
    ???
  }

  object Laws {
    val easyCheck: Prop = {
      ???
    }
  }

}
