import scala.util.matching.*

trait Parsers[ParseError, Parser[+_]]:
  extension [A, E](p: Parser[A])
    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]

    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def listOfN(n: Int): Parser[List[A]] =
      if n == 0 then unit(List.empty)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(a => unit(f(a)))

    def product[B](pb: => Parser[B]): Parser[(A, B)] =
      p.map2(pb)((_, _))

    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.flatMap(a => pb.flatMap(b => unit(f(a, b))))

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | unit(List.empty)

    def filter(predicate: A => Boolean): Parser[A]

    def slice: Parser[String]

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def ignoreWhitespace: Parser[A] =
      (whitespace *> p) <* whitespace

    def <*[B](pb: => Parser[B]): Parser[A] =
      p.pickLeft(pb)

    def *>[B](pb: => Parser[B]): Parser[B] =
      p.pickRight(pb)

    def pickLeft[B](pb: => Parser[B]): Parser[A] =
      (p ** pb).map { case (a, b) =>
        a
      }
    def pickRight[B](pb: => Parser[B]): Parser[B] =
      (p ** pb).map { case (a, b) =>
        b
      }

    def startsWith[B](pb: => Parser[B]): Parser[A] =
      pb *> p

    def followedBy[B](pb: => Parser[B]): Parser[A] =
      p <* pb

    def sepBy[B](pb: => Parser[B]): Parser[List[A]] =
      p.map2(pb.flatMap(_ => p.sepBy(pb)))(_ :: _) | unit(List.empty)

    def token: Parser[A] = p.ignoreWhitespace

    def between[B, C](before: Parser[B], after: Parser[C]): Parser[A] =
      before *> p <* after

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p.map(a => a), p)(in)

    def unitLaw[A](in: Gen[(String, A)]): Prop =
      Prop.forAll(in) { case (in, a) =>
        unit(a).run(in) == a
      }

    def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      Prop.forAll(in) { s =>
        p1.slice.run(s) match {
          case Left(value) => Left(value) == (p1 ** p2).run(s)
          case Right(considered) => {
            val toBeConsideredP2 = s.stripPrefix(considered)
            (p1.run(considered), p2.run(toBeConsideredP2)) == (p1 ** p2).run(s)
          }
        }

      }

  def unit[A](a: A): Parser[A] =
    string("").map(_ => a)
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String]
  def countAs: Parser[Int] = char('a').many.slice.map(_.size)
  def numLetter: Parser[String] =
    regex("\\d".r).flatMap(n => char('a').listOfN(n.toInt)).map(_.mkString)

  def digit: Parser[Int] =
    regex("\\d".r).map(_.toInt)

  def stringLiteral: Parser[String] =
    regex("\"([^\"]*)\"".r).map(s => s.substring(1, s.length - 1))

  def whitespace: Parser[String] =
    regex("\\s*".r)

  def regex(r: Regex): Parser[String]

  def maybe(c: Char): Parser[Char] =
    char(c) | unit(c)

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

def parseJson[Err, Parser[+_]](
    P: Parsers[Err, Parser]
): Parser[JSON] =
  import P.*

  def json: Parser[JSON] =
    parseJsonObject | parseJsonArray | parseNum | parseString | parseBool | parseNull

  def parseKeyValue: Parser[(String, JSON)] =
    stringLiteral.token
      .followedBy(char(':').token)
      ** parseJson(P)

  def parseJsonObject: Parser[JSON] =
    parseKeyValue
      .sepBy(char(',').token)
      .map(elements => JSON.JObject(elements.toMap))
      .between(char('{').token, char('}').token)

  def parseJsonArray: Parser[JSON] =
    parseJson(P)
      .sepBy(char(',').token)
      .map(elements => JSON.JArray(elements.toIndexedSeq))
      .between(char('[').token, char(']').token)

  def parseNum: Parser[JSON] =
    (regex("-?(0|[1-9]\\d*)(\\.\\d+)?([eE][+-]?\\d+)?".r)).token.map(s =>
      JSON.JNumber(s.toDouble)
    )
  def parseNull: Parser[JSON] =
    string("null").token.map(_ => JSON.JNull)

  def parseString: Parser[JSON] =
    stringLiteral.token.map(JSON.JString(_))

  def parseBool: Parser[JSON] =
    (string("true") | string("false")).token
      .map(s => JSON.JBool(s.toBoolean))

  json.token
