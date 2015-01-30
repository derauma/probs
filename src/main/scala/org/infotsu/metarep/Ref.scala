package org.infotsu.metarep

class UnknownRefException(ref: String) extends Exception;

case class LiteralRef[T](ref: String) extends Meta[T] {

  override def gen() = Meta.get[T](ref)
}
