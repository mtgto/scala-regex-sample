package net.mtgto.regexsample

import org.specs2.mutable._

class ParserSpec extends Specification {
  "Parser" should {
    val parser = Parser

    "parse valid regular expressions" in {
      "single character" in {
        parser.parse("a").successful must beTrue
        parser.parse("b").successful must beTrue
        parser.parse("z").successful must beTrue
        parser.parse("A").successful must beTrue
      }

      "concatenate characters" in {
        parser.parse("az").successful must beTrue
        parser.parse("bcdefg").successful must beTrue
      }

      "alternate expression" in {
        parser.parse("a|b").successful must beTrue
        parser.parse("a|b|c").successful must beTrue
        parser.parse("abc|d").successful must beTrue
        parser.parse("ab|cd").successful must beTrue
        parser.parse("ab|cdef|ghi").successful must beTrue
      }

      "paren expression" in {
        parser.parse("(a)").successful must beTrue
      }

      "star expression" in {
        parser.parse("a*").successful must beTrue
        parser.parse("(a)*").successful must beTrue
        parser.parse("(ab)*").successful must beTrue
        parser.parse("(a|b)*").successful must beTrue
      }
    }

    "not parse invalid regular expressions" in {
      "empty" in {
        parser.parse("").successful must beFalse
      }

      "not English character" in {
        parser.parse("1").successful must beFalse
        parser.parse(" ").successful must beFalse
        parser.parse("あ").successful must beFalse
      }

      "empty alternate expression" in {
        parser.parse("|").successful must beFalse
        parser.parse("a|").successful must beFalse
        parser.parse("|a").successful must beFalse
        parser.parse("a||b").successful must beFalse
      }

      "empty paren" in {
        parser.parse("()").successful must beFalse
      }

      "unmatched paren" in {
        parser.parse("(a").successful must beFalse
        parser.parse("a)").successful must beFalse
        parser.parse("((a)").successful must beFalse
        parser.parse("(a)b)c(d").successful must beFalse
      }

      "empty star expression" in {
        parser.parse("*").successful must beFalse
        parser.parse("a**").successful must beFalse
      }
    }
  }
}
