package org.infotsu.metarep

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

class RefSpec 
	extends FlatSpec with Matchers {

  "Ref Int" should "retrieve a registered Integer" in {
    
    Meta.register("name1", 45)
    val ref = LiteralRef[Int]("name1")
    val got = Meta.get[Int]("name1")
    assert(ref.gen()==45)
  }  

  "Ref Literal Float" should "retrieve a registered Literal Float" in {

    Meta.register("name2", Literal[Float](45f))
    val ref2 = LiteralRef[Literal[Float]]("name2")
    val got2 = Meta.get[Literal[Float]]("name2")
    val ref3 =ref2.gen()
    assert(ref2.gen()==Literal[Float](45f))
    assert(ref3.gen()==45f)
  }

}