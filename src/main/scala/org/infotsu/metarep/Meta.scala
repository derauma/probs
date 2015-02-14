package org.infotsu.metarep

import scala.util.Random

abstract class Meta[T] {

  def gen(): T
}


case class Literal[T](t: T) extends Meta[T] {

  override def gen() = t
}


case class Choice[T](metas: List[Meta[T]]) extends Meta[T] {
  
  assert(metas.nonEmpty)

  override def gen() = { metas(Meta.ran.nextInt(metas.length)).gen() }
}

case class Op[T](f: (T,T) => T,metas: List[Meta[T]]) extends Meta[T] {
  
  assert(metas.nonEmpty)

  override def gen() = { 
    val Vals = metas map { m => m.gen }
    Vals.reduceLeft[T](f)
  }
}


case class Condition[T](meta: Meta[T], f: (T => Boolean)) extends Meta[T] {
  
  override def gen() = {
    var tries = Meta.tries
    var t=meta.gen()
    while (tries > 0) {
      if (f(t)) tries=0
      else if (tries==0) throw new MaxRetriesException()
      else {
        t=meta.gen()
        tries = tries - 1
      }
    }
    t
  }
}


object Meta	{
  
  import collection.mutable.Map

  val tries = 1000
  val ran = new Random(System.currentTimeMillis())

  private var Registry =
    Map.empty[String,Any]

  def register[T](ref: String, value: T) =
    Registry.put(ref, value)

  def get[T](ref: String): T = {
    Registry.get(ref) match {
      case Some(x) => x.asInstanceOf[T]
      case None => throw new UnknownRefException(ref)
    }
  }
      
  implicit def Any2Literal[T](t: T) = Literal[T](t)
}

class MaxRetriesException extends Exception;


