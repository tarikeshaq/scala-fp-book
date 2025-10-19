import scala.util.matching.*


trait Parsers[ParseError, Parser[+_]]:
  extension [A, E](p: Parser[A])
    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]

    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def listOfN(n: Int): Parser[List[A]] =
      if n == 0 then unit(List.empty)
      else p.map2(p.listOfN(n-1))(_ :: _)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(a => unit(f(a)))

    def product[B](pb: => Parser[B]): Parser[(A, B)] =
      p.map2(pb)((_, _))

    def **[B](p2: => Parser[B]): Parser[(A, B) ] = product(p2)

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.flatMap(a => pb.flatMap(b => unit(f(a, b)))) 

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | unit(List.empty)

    def filter(predicate: A => Boolean): Parser[A]

    def slice: Parser[String]

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def ignoreWhitespace: Parser[A] =
      ((whitespace ** p) ** whitespace).map {
        case ((_, a), _) => a
      }


  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p.map(a => a), p)(in)

    def unitLaw[A](in: Gen[(String, A)]): Prop =
      Prop.forAll(in) {
        case (in, a) => unit(a).run(in) == a
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
    regex("\"([^\"]*)\"".r)

  def whitespace: Parser[String] =
    (char(' ') | char('\n')).many.map(_.mkString) | unit("")


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

    def parseKeyValue: Parser[(String, JSON)] =
      (stringLiteral.ignoreWhitespace ** char(':').ignoreWhitespace ** parseJson(P) ** maybe(',')).map {
        case (((s, _), j), _) => (s, j)
      }

    def parseJsonObject: Parser[JSON] =
      (char('{').ignoreWhitespace
      **
      parseKeyValue.many.map(elements => JSON.JObject(elements.toMap))
      **
      char('}').ignoreWhitespace).map {
        case ((_, res), _) => res
      }

    def parseArrayElem: Parser[JSON] =
      (parseJson(P).ignoreWhitespace ** maybe(',')).map {
        case (j, _) => j
      }

    def parseJsonArray: Parser[JSON] =
      (
        char('[').ignoreWhitespace
        **
        parseArrayElem.many.map(elements => JSON.JArray(elements.toIndexedSeq))
        **
        char(']').ignoreWhitespace
      ).map {
        case ((_, j), _) => j
      }

    def parseNum: Parser[JSON] =
      (regex("\\d".r) | char('.') | char('e')).ignoreWhitespace.many.map(l => JSON.JNumber(l.mkString.toDouble))

    def parseNull: Parser[JSON] =
      string("null").map(_ => JSON.JNull)

    def parseString: Parser[JSON] =
      stringLiteral.ignoreWhitespace.map(JSON.JString(_))

    def parseBool: Parser[JSON] =
      (string("true") | string("false")).ignoreWhitespace.map(_.toBoolean).map(JSON.JBool(_))


    parseJsonObject | parseJsonArray | parseString | parseNum | parseBool | parseNull 

