package net.mtgto.regexsample

sealed trait Pattern
// 一文字
case class Ch(c: Char) extends Pattern
// A | B
case class Or(a: Seq[Pattern]) extends Pattern
// A B
case class And(a: Seq[Pattern]) extends Pattern
// A *
case class Repeat(a: Pattern) extends Pattern
