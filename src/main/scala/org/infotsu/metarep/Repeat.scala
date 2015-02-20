package org.infotsu.metarep

case class Repeat[T](meta: Meta[T], mcount: Meta[Int]) {

  def gen(): List[T] = {
    
    val cnt = mcount.gen()
    var Res: List[T] = List[T]()

    for (i <- Range(0, cnt)) {
      Res = meta.gen :: Res
    }
    Res
  }
}