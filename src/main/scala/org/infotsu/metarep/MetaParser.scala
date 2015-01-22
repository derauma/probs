package org.infotsu.metarep

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers

class ParseException(val msg: String) extends Exception;

trait MetaParserDoubleT extends JavaTokenParsers
  with PackratParsers {

  lazy val term_dbl: PackratParser[Meta[Double]] = expr_dbl | choice_dbl | random_cont | literal_dbl

  lazy val choice_dbl: PackratParser[Meta[Double]] = "(" ~> rep1sep(term_dbl, "|") <~ ")" ^^ { case terms: List[Meta[Double]] => Choice[Double](terms) }

  lazy val expr_dbl: PackratParser[Meta[Double]] = term_dbl ~ infix_op ~ term_dbl ^^ {
    case left ~ op ~ right =>

      op match {
        case "*" => new Op((x: Double, y: Double) => x * y, List(left, right))
        case "/" => new Op((x: Double, y: Double) => x / y, List(left, right))
        case "+" => new Op((x: Double, y: Double) => x + y, List(left, right))
        case "-" => new Op((x: Double, y: Double) => x - y, List(left, right))
      }
  }

  lazy val random_cont: PackratParser[Meta[Double]] = uniform | gaussian | exp

  lazy val uniform: PackratParser[Meta[Double]] = floatingPointNumber ~ "to" ~ floatingPointNumber ^^ { case from ~ _ ~ to => new RUniform(from.toDouble, to.toDouble) }
  lazy val gaussian: PackratParser[Meta[Double]] = "normal(" ~> floatingPointNumber ~ "," ~ floatingPointNumber <~ ")" ^^ { case mu ~ _ ~ sigma => new RGaussian(mu.toDouble, sigma.toDouble) }
  lazy val exp: PackratParser[Meta[Double]] = "exp(" ~> floatingPointNumber <~ ")" ^^ { case rate => new RExp(rate.toDouble) }

  lazy val literal_dbl: PackratParser[Meta[Double]] = floatingPointNumber ^^ { case f => Literal[Double](f.toDouble) }

  val infix_op: PackratParser[String] = "+" | "-" | "*" | "/"


}

class MetaParserDouble extends MetaParserDoubleT {
  
    def parse(text: String): Meta[Double] = {

    parseAll(term_dbl, text) match {

      case Success(meta, _) => meta
      case Failure(msg, _) =>
        println("Failed parsing '" + text + "': " + msg); throw new ParseException(msg)
      case Error(msg, _) => println("Error parsing '" + text + "': " + msg); throw new ParseException(msg)
    }
  }
}

trait MetaParserIntT extends JavaTokenParsers
  with PackratParsers {

  lazy val term_int: PackratParser[Meta[Int]] = expr_int | choice_int | random_disc | literal_int

  lazy val choice_int: PackratParser[Meta[Int]] = "(" ~> rep1sep(term_int, "|") <~ ")" ^^ { case terms: List[Meta[Int]] => Choice[Int](terms) }

  lazy val expr_int: PackratParser[Meta[Int]] = term_int ~ infix_op_int ~ term_int ^^ {
    case left ~ op ~ right =>

      op match {
        case "*" => new Op((x: Int, y: Int) => x * y, List(left, right))
        case "/" => new Op((x: Int, y: Int) => x / y, List(left, right))
        case "+" => new Op((x: Int, y: Int) => x + y, List(left, right))
        case "-" => new Op((x: Int, y: Int) => x - y, List(left, right))
      }
  }

  lazy val random_disc: PackratParser[Meta[Int]] = binom | poisson | geom

  lazy val binom: PackratParser[Meta[Int]] = "binom(" ~> wholeNumber ~ "," ~ floatingPointNumber <~ ")" ^^ { case n ~ _ ~ p => new RBinom(n.toInt, p.toDouble) }
  lazy val poisson: PackratParser[Meta[Int]] = "poisson(" ~> floatingPointNumber <~ ")" ^^ { case mean => new RPoisson(mean.toDouble) }
  lazy val geom: PackratParser[Meta[Int]] = "geom(" ~> floatingPointNumber <~ ")" ^^ { case p => new RGeom(p.toDouble) }

  lazy val literal_int: PackratParser[Meta[Int]] = wholeNumber ^^ { case f => Literal[Int](f.toInt) }

  val infix_op_int: PackratParser[String] = "+" | "-" | "*" | "/"
}

class MetaParserInt extends MetaParserIntT {
  
    def parse(text: String): Meta[Int] = {

    parseAll(term_int, text) match {

      case Success(meta, _) => meta
      case Failure(msg, _) =>
        println("Failed parsing '" + text + "': " + msg); throw new ParseException(msg)
      case Error(msg, _) => println("Error parsing '" + text + "': " + msg); throw new ParseException(msg)
    }
  }
}
