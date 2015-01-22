package org.infotsu.metarep


case class Literal[T](t: T) extends Meta[T] {

  //implicit def TtoLiteral(t: T) = Literal[T](t)
  
  override def gen() = t
}
