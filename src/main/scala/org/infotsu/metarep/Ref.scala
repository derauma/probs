package org.infotsu.metarep

class UnknownRefException(ref: String) extends Exception;

case class LiteralRef[T](ref: String) extends Meta[T] {

  override def gen() = Meta.get[T](ref)
}

case class Shared[T](meta: Meta[T], name: String) extends Meta[T] {
  
  override def gen() = {
    
    val res: T = meta.gen
    Meta.register(name, res)
    res
  }
}