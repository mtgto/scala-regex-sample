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

  def exp2state(exp: Exp): (State, EmptyState) = {
    exp match {
      case Ch(c) => {
        val nextState = new EmptyState
        val firstState = new CharState(c, nextState)
        (firstState, nextState)
      }
      case Or(exps) => {
        val firstState = new EmptyState
        val nextState = new EmptyState
        exps.foreach {
          exp =>
            val (fst, lst) = exp2state(exp)
          firstState.addState(fst)
          lst.addState(nextState)
        }
        (firstState, nextState)
      }
      case And(exps) => {
        exps.map(exp2state(_)) match {
          case hd :: tl => {
            tl.foldLeft(hd)(
              (s1, s2) => {
                s1._2.addState(s2._1)
                s2
              }
            )
            (hd._1, tl.last._2)
          }
        }
      }
      case Repeat(exp) => {
        val firstState = new EmptyState
        val nextState = new EmptyState
        val (fst, lst) = exp2state(exp)
        firstState.addState(fst)
        firstState.addState(nextState)
        lst.addState(fst)
        lst.addState(nextState)
        (firstState, nextState)
      }
    }
  }

  def compile(input: String): Pattern = {
    parseAll(parse0, input) match {
      case Success(result, _) => {
        val (fst, lst) = exp2state(result)
        lst.addState(FiniteState)
        new Pattern(fst)
      }
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }
}

class Pattern(state: State) {
  def matches(str: String): Boolean = {
    state.matches(str.toList)
  }

  override def toString = "Pattern(" + state.toString + ")"
}
