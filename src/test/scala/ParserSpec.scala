package net.mtgto.regexsample

import org.specs2.mutable._

class ParserSpec extends Specification {
  "Parser" should {
    val parser = new Parser

    "parse valid regular expressions" in {
      val hoge = parser.parse("a")
      println(hoge)
      hoge === 1
    }
  }
}
