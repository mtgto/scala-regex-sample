package net.mtgto.regexsample

import scala.util.parsing.combinator._

object Pattern extends RegexParsers {
  sealed trait Exp
  // 一文字
  case class Ch(c: Char) extends Exp
  // A | B
  case class Or(a: Seq[Exp]) extends Exp
  // A B
  case class And(a: Seq[Exp]) extends Exp
  // A *
  case class Repeat(a: Exp) extends Exp

  private def parse0: Parser[Exp] =
    parse1 ~ rep("|" ~> parse1) ^^ {
      case p1 ~ Nil => p1
      case p1 ~ p1s => Or(p1 :: p1s)
    }

  private def parse1: Parser[Exp] =
    parse2 ~ rep(parse2) ^^ {
      case p2 ~ Nil => p2
      case p2 ~ p2s => And(p2 :: p2s)
    }

  private def parse2: Parser[Exp] =
    parse3 <~ "*" ^^ {
      case p3 => Repeat(p3)
    } | parse3 ^^ {
      case p3 => p3
    }

  private def parse3: Parser[Exp] =
    "(" ~> parse0 <~ ")" ^^ {
      case p0 => p0
    } | """[a-zA-Z]""".r ^^ { case c => Ch(c.head) }

  def parse(input: String): ParseResult[Exp] = {
    parseAll(parse0, input)
  }
}

