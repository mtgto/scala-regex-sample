package net.mtgto.regexsample

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  private def parse0: Parser[Pattern] =
    parse1 ~ rep("|" ~> parse1) ^^ {
      case p1 ~ Nil => p1
      case p1 ~ p1s => Or(p1 :: p1s)
    }

  private def parse1: Parser[Pattern] =
    parse2 ~ rep(parse2) ^^ {
      case p2 ~ Nil => p2
      case p2 ~ p2s => And(p2 :: p2s)
    }

  private def parse2: Parser[Pattern] =
    parse3 <~ "*" ^^ {
      case p3 => Repeat(p3)
    } | parse3 ^^ {
      case p3 => p3
    }

  private def parse3: Parser[Pattern] =
    "(" ~> parse0 <~ ")" ^^ {
      case p0 => p0
    } | """[a-zA-Z]""".r ^^ { case c => Ch(c.head) }

  def parse(input: String): ParseResult[Pattern] = {
    parseAll(parse0, input)
  }
}
