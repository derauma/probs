package org.infotsu.metarep

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

class MetaParserSpec
  extends FlatSpec with Matchers {

  val parser_dbl = new MetaParserDouble
  val parser_int = new MetaParserInt

  "MetaParser" should "return a literal Double" in {

    val rep = parser_dbl.parse("3.14159")
    val res = rep.gen()
    assert(res == 3.14159)
  }

  "MetaParser" should "calculate the sum of 3 + 4" in {

    val rep = parser_dbl.parse("3 + 4")
    val res = rep.gen()
    assert(res == 7)
  }

  "MetaParser" should "return a complex Choice" in {

    val rep = parser_dbl.parse("(3 + 0.1 | 4 | 5 | -0.1)")
    val res = rep.gen()
  }

  "MetaParser" should "return an operator on Choices" in {

    val rep = parser_dbl.parse("(3 | 4) + (.1 | .2)")
    val res = rep.gen()
  }

  "MetaParser" should "give multiplication precedence over addition 1" in {

    val rep = parser_dbl.parse("1 + 2 * 2")
    assert(rep.gen() == 5)
  }

  "MetaParser" should "give multiplication precedence over addition 2" in {

    val rep = parser_dbl.parse("1 * 2 + 2")
    assert(rep.gen() == 4)
  }

  "MetaParser" should "generate uniform randoms from 45.555 to 48.999" in {

    val rep = parser_dbl.parse("45.555 to 48.999")
    var res_prev = 0.0
    for (x <- Range(1, 50)) {
      val res = rep.gen
      assert(res != res_prev)
      assert(res >= 45.555); assert(res < 48.999)
      res_prev = res
    }
  }
  "MetaParser" should "generate samples from a gaussian distribution with mu 10 and sigma 1" in {

    val rep = parser_dbl.parse("normal(10,1)")
    var res_prev = 0.0
    for (x <- Range(1, 50)) {
      val res = rep.gen
      assert(res != res_prev)
      //println("gaussian(10,1) is "+res)
      res_prev = res
    }
  }
  "MetaParser" should "generate samples from an exponential distribution with rate 1.1" in {

    val rep = parser_dbl.parse("exp(1.1)")
    var res_prev = 0.0
    for (x <- Range(1, 50)) {
      val res = rep.gen
      assert(res != res_prev)
      println("exp(1.1) is " + res)
      res_prev = res
    }
  }

  "MetaParser" should "generate samples from a binomial distribution with n 5 and p 0.2" in {

    val rep = parser_int.parse("binom(5,0.2)")
    var res_prev = 0.0
    for (x <- Range(1, 50)) {
      val res = rep.gen
      //assert(res != res_prev)
      println("binom(5,0.2) is " + res)
      res_prev = res
    }
  }

  "MetaParser" should "generate samples from a poisson distribution with mean 38" in {

    val rep = parser_int.parse("poisson(38)")
    var res_prev = 0.0
    for (x <- Range(1, 50)) {
      val res = rep.gen
      //assert(res != res_prev)
      println("poisson(38) is " + res)
      res_prev = res
    }
  }

  "MetaParser" should "generate samples from a geometric distribution with p 0.05" in {

    val rep = parser_int.parse("geom(0.05)")
    var res_prev = 0.0
    for (x <- Range(1, 50)) {
      val res = rep.gen
      //assert(res != res_prev)
      println("geom(0.05) is " + res)
      res_prev = res
    }
  }
}
