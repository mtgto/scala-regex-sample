package net.mtgto.regexsample

import org.specs2.mutable._

class PatternSpec extends Specification {
  "Pattern" should {
    val pattern = Pattern

    "parse valid regular expressions" in {
      "single character" in {
        pattern.parse("a").successful must beTrue
        pattern.parse("b").successful must beTrue
        pattern.parse("z").successful must beTrue
        pattern.parse("A").successful must beTrue
      }

      "concatenate characters" in {
        pattern.parse("az").successful must beTrue
        pattern.parse("bcdefg").successful must beTrue
      }

      "alternate expression" in {
        pattern.parse("a|b").successful must beTrue
        pattern.parse("a|b|c").successful must beTrue
        pattern.parse("abc|d").successful must beTrue
        pattern.parse("ab|cd").successful must beTrue
        pattern.parse("ab|cdef|ghi").successful must beTrue
      }

      "paren expression" in {
        pattern.parse("(a)").successful must beTrue
      }

      "star expression" in {
        pattern.parse("a*").successful must beTrue
        pattern.parse("(a)*").successful must beTrue
        pattern.parse("(ab)*").successful must beTrue
        pattern.parse("(a|b)*").successful must beTrue
      }
    }

    "not parse invalid regular expressions" in {
      "empty" in {
        pattern.parse("").successful must beFalse
      }

      "not English character" in {
        pattern.parse("1").successful must beFalse
        pattern.parse(" ").successful must beFalse
        pattern.parse("あ").successful must beFalse
      }

      "empty alternate expression" in {
        pattern.parse("|").successful must beFalse
        pattern.parse("a|").successful must beFalse
        pattern.parse("|a").successful must beFalse
        pattern.parse("a||b").successful must beFalse
      }

      "empty paren" in {
        pattern.parse("()").successful must beFalse
      }

      "unmatched paren" in {
        pattern.parse("(a").successful must beFalse
        pattern.parse("a)").successful must beFalse
        pattern.parse("((a)").successful must beFalse
        pattern.parse("(a)b)c(d").successful must beFalse
      }

      "empty star expression" in {
        pattern.parse("*").successful must beFalse
        pattern.parse("a**").successful must beFalse
      }
    }
  }
}
