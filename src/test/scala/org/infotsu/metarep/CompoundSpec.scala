package org.infotsu.metarep

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.immutable.Range

class CompoundSpec
  extends FlatSpec with Matchers {

  case class Compound(id: Int, name: String, value: Double) {

    override def toString = "id=" + id + " name='" + name + "' value=" + value
    def +(c1: Compound): Compound = { Compound(0, this.name + c1.name, this.value + c1.value) }

  }

  def +(c1: Compound, c2: Compound): Compound = { Compound(0, c1.name + c2.name, c1.value + c2.value) }

  case class MCompound(id: Meta[Int], name: Meta[String], value: Meta[Double]) extends Meta[Compound] {

    override def gen() = Compound(id.gen, name.gen, value.gen)

  }

  class MetaParserCompound extends MetaParserDouble
    with MetaParserIntT {

    lazy val term: PackratParser[Meta[Compound]] = expr | choice | literal | mcompound

    lazy val choice: PackratParser[Meta[Compound]] = "(" ~> rep1sep(term, "|") <~ ")" ^^ { case terms: List[Meta[Compound]] => Choice[Compound](terms) }

    lazy val expr: PackratParser[Meta[Compound]] = term ~ infix_op ~ term ^^ {
      case left ~ op ~ right =>

        op match {
          case "+" => Op((x: Compound, y: Compound) => x + y, List(left, right))
        }
    }

    lazy val literal: PackratParser[Meta[Compound]] = "[" ~> wholeNumber ~ ident ~ floatingPointNumber <~ "]" ^^ { case id ~ name ~ value => Literal[Compound](Compound(id.toInt, name, value.toDouble)) }

    lazy val mcompound: PackratParser[Meta[Compound]] = term_int ~ ident ~ term_dbl ^^ { case id ~ name ~ dbl => MCompound(id, Literal[String](name), dbl) }

    def apply(text: String): Meta[Compound] = {

      parseAll(term, text) match {

        case Success(meta, _) => meta
        case Failure(msg, _) =>
          info("Failed parsing '" + text + "': " + msg); throw new ParseException(msg)
        case Error(msg, _) => info("Error parsing '" + text + "': " + msg); throw new ParseException(msg)
      }
    }
  }

      val parser = new MetaParserCompound

  "MetaParserCompound" should "parse a literal compound" in {


    val rep = parser("21 my_name1 45.667")
    info("Generation of parsed literal compound is '" + rep.gen + "'")
  }

  "MetaParserCompound" should "parse an mcompound with operators and choices" in {

    val rep = parser("(22 my_name2 4.5 + (6 | 38) | [1 fixed 2.222])")
    for (x <- Range(1, 10)) {
      info("Generation of parsed mcompound is '" + rep.gen + "'")
    }
  }

  "MetaParserCompound" should "parse an mcompound with operators and random functions" in {

    val rep = parser("23 my_name3 48 + normal(0,0.3)")
    for (x <- Range(1, 10)) {
      info("Generation of parsed mcompound is '" + rep.gen + "'")
    }
  }

  val id = 444

  val name = Choice[String](List("me", "you", "them"))

  val value_rep = RGaussian(0, 1)

  "Literal Compound" should "be created" in {

    val lit = Literal[Compound](Compound(0, "literal compound", 0))
  }

  "Literal Compound" should "be created implicitly" in {

    val lit: Literal[Compound] = Compound(0, "implicit literal compound", 0)
  }

  "MCompound" should "generate compounds" in {

    val comp_rep = MCompound(id, name, value_rep)

    for (x <- Range(1, 10)) {
      info(comp_rep.gen.toString)
    }
  }

  "MCompound" should "have value greater than 1.6 when name='them'" in {

    val id2 = 555
    val comp_rep = MCompound(id2, name, value_rep)

    def cond(c: Compound) = { if (c.name == "them") (c.value > 1.6) else true }

    val cond_rep = Condition(comp_rep, cond)

    for (x <- Range(1, 10)) {

      val gen = cond_rep.gen
      assert(cond(gen))
      info(gen.toString)
    }
  }
}
