package org.infotsu.metarep

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

class NumericSpec 
	extends FlatSpec with Matchers {

  "Float Literal" should "instantiate a literal float" in {
    
    val literal_float = Literal[Float](1.0f)
    assert(literal_float.gen == 1.0f)
  }
  
  "Float choice" should "instantiate three literal choices" in {
    
    val literal_choice = Choice[Float](List(Literal[Float](1.1f),
        Literal[Float](2.2f),Literal[Float](3.3f)))
    val choice = literal_choice.gen
  }

  "Float Choice" should "instantiate a tree of choices" in {

    val choice3 = Choice[Float](List(Literal[Float](1.1f),
      Literal[Float](2.2f), Literal[Float](3.3f)))

    val literal = Literal[Float](4.4f)
    val choice_top = Choice[Float](List(literal, choice3))
    val choice = choice_top.gen
    println("Choice is " + choice)
  }
    
  "Double Uniform" should "return a uniform sample between 5 and 10" in {
    
    val uniform = new RUniform(5,10)
    println("Uniform sample: ")
    for ( i <- Range(0,5) ) print(uniform.gen+" ")
    assert(uniform.gen >= 5)
    assert(uniform.gen < 10)
  }
  
  "Double Gaussian" should "return a gaussian sample with mean 10 and stdev 5" in {
    
    val gaussian = new RGaussian(10,2)
    println("Gaussian sample: ")
    for ( i <- Range(0,5) ) print(gaussian.gen+" ")
  }
  
  "Double Poisson" should "return a poisson sample" in {
    
    val poisson = new RPoisson(10)
    println("Poisson sample: ")
    for ( i <- Range(0,5) ) print(poisson.gen+" ")
  }

  "Double Exponential" should "return an exponential sample" in {
    
    val exp = new RExp(10)
    println("Exponential sample: ")
    for ( i <- Range(0,5) ) print(exp.gen+" ")
  }

  "Double Binomial" should "return a binomial sample" in {
    
    val binom = new RBinom(10,0.1)
    println("Binomial sample: ")
    for ( i <- Range(0,5) ) print(binom.gen+" ")
  }
  
  "Double Geometric" should "return a geometric sample" in {
    
    val geom = new RGeom(0.4)
    println("Geometric sample: ")
    for ( i <- Range(0,5) ) print(geom.gen+" ")
  }
    
  "Op plus" should "add two doubles" in {
    
    val d1 = Literal[Double](1)
    val d2 = Literal[Double](1)
    
    def plus_double(t1: Double, t2: Double) = t1 + t2

    val plus_rep = new Op[Double](plus_double,List(d1,d2))
    assert(plus_rep.gen()==2.0)
  }
  
  "Op max" should "return the max value in a list" in {
    
    val d1 = Literal[Double](2)
    val d2 = Literal[Double](1)
    val d3 = Literal[Double](3)
    val d4 = Literal[Double](2.5)
    
    def max_double(t1: Double, t2: Double) = { if (t1 > t2) t1 else t2 }

    val max_rep = new Op[Double](max_double,List(d1,d2,d3,d4))
    assert(max_rep.gen()==3)
  }
}