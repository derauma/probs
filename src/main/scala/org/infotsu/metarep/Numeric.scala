package org.infotsu.metarep

import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Poisson
import breeze.stats.distributions.Uniform
import breeze.stats.distributions.Exponential
import breeze.stats.distributions.Geometric

 // continuous distributions

class RUniform(min: Double, max: Double) extends Meta[Double] {
  
  val uniform = new Uniform(min,max)
  override def gen = uniform.draw()
}
import breeze.stats.distributions.Binomial

class RGaussian(mu: Double, sigma: Double) extends Meta[Double] {
  
  val gaussian = new Gaussian(mu,sigma)
  override def gen = gaussian.draw()
}

class RExp(rate: Double) extends Meta[Double] {
  
  val exp = new Exponential(rate)
  override def gen = exp.draw()
}

 // discrete distributions

class RBinom(n: Int, p: Double) extends Meta[Int] {
  
  val binom = new Binomial(n,p)
  override def gen = binom.draw()
}

class RPoisson(mean: Double) extends Meta[Int] {
  
  val poisson = new Poisson(mean)
  override def gen = poisson.draw()
}

class RGeom(p: Double) extends Meta[Int] {
  
  val geom = new Geometric(p)
  override def gen = geom.draw()
}
