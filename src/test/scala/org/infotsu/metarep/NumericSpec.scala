package org.infotsu.metarep

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class NumericSpec
  extends FlatSpec with Matchers {

  "Float Literal" should "instantiate a literal float" in {

    val literal_float = Literal[Float](1.0f)
    literal_float.gen should equal(1.0f)
  }

  "Float Literal" should "instantiate an implicit literal float" in {

    val literal_float: Literal[Float] = 1.0f
    literal_float.gen should equal(1.0f)

  }

  "Float choice" should "instantiate three literal choices" in {

    val literal_choice = Choice[Float](List(1.1f, 2.2f, 3.3f))
    val choice = literal_choice.gen
  }

  "Float Choice" should "instantiate a tree of choices" in {

    val choice3 = Choice[Float](List(1.1f, 2.2f, 3.3f))

    val choice_top = Choice[Float](List(4.4f, choice3))
    val choice = choice_top.gen
    info("Choice is " + choice)
  }

  "Double Uniform" should "return a uniform sample between 5 and 10" in {

    val uniform = RUniform(5, 10)
    info("Uniform sample: ")
    for (i <- Range(0, 5)) {
      val gen1 = uniform.gen
      info(gen1 + " ")
      gen1 should be >= 5.0
      gen1 should be < 10.0
    }
  }
  
  "Double Dynamic Uniform" should "return a uniform sample from a variable min of 1 to 5 to a variable max of 10 to 20" in {
    
    val var_uniform = RUniform(RUniform(1,5),RUniform(10,20))
    info("Uniform sample: ")
    for (i <- Range(0, 5)) {
      val gen1 = var_uniform.gen
      info(gen1 + " ")
      gen1 should be >= 1.0
      gen1 should be < 20.0
    }
  }

  "Double Gaussian" should "return a gaussian sample with mean 10 and stdev 5" in {

    val gaussian = RGaussian(10, 2)
    info("Gaussian sample: ")
    for (i <- Range(0, 5)) info(gaussian.gen + " ")
  }

  "Double Poisson" should "return a poisson sample" in {

    val poisson = RPoisson(10)
    info("Poisson sample: ")
    for (i <- Range(0, 5)) info(poisson.gen + " ")
  }

  "Double Exponential" should "return an exponential sample" in {

    val exp = RExp(10)
    info("Exponential sample: ")
    for (i <- Range(0, 5)) info(exp.gen + " ")
  }

  "Double Binomial" should "return a binomial sample" in {

    val binom = RBinom(10, 0.1)
    info("Binomial sample: ")
    for (i <- Range(0, 5)) info(binom.gen + " ")
  }

  "Double Geometric" should "return a geometric sample" in {

    val geom = RGeom(0.4)
    info("Geometric sample: ")
    for (i <- Range(0, 5)) info(geom.gen + " ")
  }

  "Op plus" should "add two Literal Doubles" in {

    val d1: Literal[Double] = 1
    val d2 = Literal[Double](1)

    def plus_double(t1: Double, t2: Double) = t1 + t2

    val plus_rep = Op[Double](plus_double, List(d1, d2))
    plus_rep.gen should equal(2.0)
  }

  "Op max" should "return the max value in a list" in {

    def max_double(t1: Double, t2: Double) = { if (t1 > t2) t1 else t2 }

    val max_rep = Op[Double](max_double, List(2, 4.444, 3, 2.5))
    max_rep.gen should equal(4.444)
  }
}