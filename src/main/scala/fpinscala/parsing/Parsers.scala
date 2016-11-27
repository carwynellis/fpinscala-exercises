package fpinscala.parsing

import fpinscala.testing.Prop._
import fpinscala.testing.{Gen, Prop}

import scala.language.{higherKinds, implicitConversions}

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, nonStrict(many(p)))(_ :: _) or succeed(List.empty[A])

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, nonStrict(many(p)))(_ :: _)

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    // Note - tupled takes care of unpacking the tuple into discrete arguments
    //        to the function avoiding tuple boilerplate
    //        e.g. { t => f(t._1, t._2) }
    map(product(p, nonStrict(p2)))(f.tupled)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 1) map2(p, nonStrict(listOfN(n - 1, p)))(_ :: _)
    else succeed(List.empty[A])

  def nonStrict[A](p: => Parser[A]): Parser[A] = p

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, nonStrict(p2))

    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, nonStrict(p2))
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in, "equal")(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}