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
        pattern.parse("(a|b)c(de)").successful must beTrue
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

    "match" in {
      "single character" in {
        val regex = pattern.compile("a")
        regex.matches("a") must beTrue
        regex.matches("") must beFalse
        regex.matches("b") must beFalse
        regex.matches("ab") must beFalse
      }

      "concatenate characters" in {
        val r1 = pattern.compile("az")
        r1.matches("az") must beTrue
        r1.matches("") must beFalse
        r1.matches("a") must beFalse
        r1.matches("z") must beFalse
        r1.matches("aaz") must beFalse
        r1.matches("aza") must beFalse
        r1.matches("azz") must beFalse

        val r2 = pattern.compile("bcdefg")
        r2.matches("bcdefg") must beTrue
      }
      
      "alternate expression" in {
        val r1 = pattern.compile("a|b")
        r1.matches("a") must beTrue
        r1.matches("b") must beTrue
        r1.matches("") must beFalse
        r1.matches("c") must beFalse
        r1.matches("aa") must beFalse
        r1.matches("bb") must beFalse
        r1.matches("ba") must beFalse

        val r2 = pattern.compile("a|b|c")
        r2.matches("a") must beTrue
        r2.matches("b") must beTrue
        r2.matches("c") must beTrue
        r2.matches("") must beFalse
        r2.matches("d") must beFalse

        val r3 = pattern.compile("abc|d")
        r3.matches("abc") must beTrue
        r3.matches("d") must beTrue
        r3.matches("a") must beFalse
        r3.matches("ab") must beFalse
        r3.matches("abcd") must beFalse

        val r4 = pattern.compile("ab|cd")
        r4.matches("ab") must beTrue
        r4.matches("cd") must beTrue

        val r5 = pattern.compile("ab|cdef|ghi")
        r5.matches("ab") must beTrue
        r5.matches("cdef") must beTrue
        r5.matches("ghi") must beTrue
      }

      "paren expression" in {
        val r1 = pattern.compile("(a)")
        r1.matches("a") must beTrue
        r1.matches("") must beFalse
        
        val r2 = pattern.compile("(a|b)c(de)")
        r2.matches("acde") must beTrue
        r2.matches("bcde") must beTrue
        r2.matches("cde") must beFalse
        r2.matches("abcde") must beFalse
      }

      "star expression" in {
        val r1 = pattern.compile("a*")
        r1.matches("") must beTrue
        r1.matches("a") must beTrue
        r1.matches("aa") must beTrue
        r1.matches("aaa") must beTrue
        r1.matches("b") must beFalse

        val r2 = pattern.compile("(a)*")
        r2.matches("") must beTrue
        r2.matches("a") must beTrue
        r2.matches("aa") must beTrue
        r2.matches("aaa") must beTrue
        r2.matches("b") must beFalse

        val r3 = pattern.compile("(ab)*")
        r3.matches("") must beTrue
        r3.matches("ab") must beTrue
        r3.matches("abab") must beTrue
        r3.matches("ababab") must beTrue
        r3.matches("a") must beFalse

        val r4 = pattern.compile("(a|b)*")
        r4.matches("") must beTrue
        r4.matches("a") must beTrue
        r4.matches("b") must beTrue
        r4.matches("ab") must beTrue
        r4.matches("ba") must beTrue
        r4.matches("aa") must beTrue
        r4.matches("bb") must beTrue
        r4.matches("aba") must beTrue
      }

      "sample expression" in {
        val r = pattern.compile("b(a|u)lls*")
        r.matches("ball") must beTrue
        r.matches("bull") must beTrue
        r.matches("balls") must beTrue
        r.matches("bulls") must beTrue
        r.matches("bullss") must beTrue
        r.matches("ballsss") must beTrue
      }
    }
  }
}
