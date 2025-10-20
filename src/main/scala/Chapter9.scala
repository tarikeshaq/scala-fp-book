import scala.util.matching.*
case class ParseError(stack: List[(Location, String)])

trait Parsers[Parser[+_]]:
  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]

    def label(msg: String): Parser[A]

    def scope(msg: String): Parser[A]

    def attempt: Parser[A]

    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def listOfN(n: Int): Parser[List[A]] =
      if n == 0 then unit(List.empty)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(a => unit(f(a)))

    def product[B](pb: => Parser[B]): Parser[(A, B)] =
      p.map2(pb)((_, _))

    def **[B](p2: => Parser[B]): Parser[(A, B)] = p.product(p2)

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.flatMap(a => pb.flatMap(b => unit(f(a, b))))

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | unit(List.empty)


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
      p.map2((pb *> p.sepBy(pb)) | unit(List.empty))(_ :: _) | unit(List.empty)

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

  def unit[A](a: A): Parser[A]
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
    regex("\\s*".r)

  def regex(r: Regex): Parser[String]

  def maybe(c: Char): Parser[Char] =
    char(c) | unit(c)

case class Location(input: String, offset: Int = 0):
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
    case -1        => offset + 1
    case lineStart => offset - lineStart

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

def parseJson[Parser[+_]](
    P: Parsers[Parser]
): Parser[JSON] =
  import P.*

  def json: Parser[JSON] =
    parseJsonObject | parseJsonArray | parseNum | parseString | parseBool | parseNull

  def parseKeyValue: Parser[(String, JSON)] =
    stringLiteral.token
      .followedBy(char(':').token)
      ** json 

  def parseJsonObject: Parser[JSON] =
    parseKeyValue
      .sepBy(char(',').token)
      .map(elements => JSON.JObject(elements.toMap))
      .between(char('{').token, char('}').token)

  def parseJsonArray: Parser[JSON] =
    json
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

  json.token.label("failed to parse json")

opaque type ParserImpl[+A] = (
    String
) => (Int, List[(Location, String)], Either[ParseError, A])

object ParserImpl:
  def apply[A](
      f: (String) => (Int, List[(Location, String)], Either[ParseError, A])
  ): ParserImpl[A] = f

  extension [A](p: ParserImpl[A])
    def eval(input: String): (Int, List[(Location, String)], Either[ParseError, A]) =
      p(input)

object ParsersImpl extends Parsers[ParserImpl]:
    extension [A](p: ParserImpl[A])
        def run(input: String): Either[ParseError, A] =
          val (offset, errList, res) = p.eval(input)
          res

        def slice: ParserImpl[String] = 
          ParserImpl.apply(
            input =>
              val (offset, errList, res) = p.eval(input)
              res match {
                case Left(err) => (offset, errList, Left(err))
                case Right(_) => (offset, errList, Right(input.slice(0, offset)))
              }
               
          )

        def label(msg: String): ParserImpl[A] = 
         ParserImpl.apply(
           input =>
             val (offset, oldErrList, res) = p.eval(input)
             res match {
               case Right(_) => (offset, oldErrList, res)
               case Left(err) => {
                   val errList = (Location(input, offset), msg) :: oldErrList
                   (offset, errList, Left(ParseError(errList)))
                }         
             } 
            )
        def scope(msg: String): ParserImpl[A] =
          ParserImpl.apply(
            input =>
              val (offset, oldErrList, res) = p.eval(input)
              val errList = (Location(input, offset), msg) :: oldErrList
              res match {
               case Right(_) => (offset, errList, res)
               case Left(err) => {
                   (offset, errList, Left(ParseError(errList)))
                }         
             } 
          )

        def flatMap[B](f: A => ParserImpl[B]): ParserImpl[B] = 
          ParserImpl.apply(
            input =>
              val (offset, errList, res) = p.eval(input)
              res match {
                case Right(value) => {
                  val nextParser = f(value)
                  val (nextOffset, nextErrList, nextRes) = nextParser.eval(input.slice(offset, input.size))
                  val finalErrList = errList ++ nextErrList
                  val finalOffset = offset + nextOffset 
                  nextRes match { 
                  case Right(_) => (finalOffset, finalErrList, nextRes)
                  case Left(_) => (finalOffset, finalErrList, Left(ParseError(finalErrList)))
                  }
                } 
                case Left(e) => (offset, errList, Left(e))
              }
          )
        
        def attempt: ParserImpl[A] = ParserImpl.apply(
           input =>
             val (offset, errList, res) = p.eval(input)
             res match {
               case Right(_) => (offset, errList, res)
               case Left(err) => (0, errList, Left(err))
             }
        )

        infix def or(p2: => ParserImpl[A]): ParserImpl[A] =
          ParserImpl.apply(
            input =>
              val (offset, errList, res) = p.eval(input)
              res match {
                case Right(value) => (offset, errList, res)
                case Left(err) => {
                   if offset != 0 then (offset, errList, res) 
                   else
                    p2.eval(input)
                }
              }

          )



    def string(s: String): ParserImpl[String] =
    ParserImpl.apply((input: String) =>
      def parseChars(s: List[Char], input: List[Char]): (Int, Boolean) =
        (s, input) match {
          case ((firstExp :: restExp), (firstInput :: restInput))
              if firstExp == firstInput => {
                val (offsetRes, success) = parseChars(restExp, restInput)
                (1 + offsetRes, success)
              }
          case (Nil, _) => (0, true)
          case _ => (0, false)
        }

      val (offsetRead, success) =
        parseChars(s.toCharArray().toList, input.toCharArray().toList)

      if success then
        val res: Either[ParseError, String] = Right(s)
        (offsetRead, List[(Location, String)](), res)
      else 
       val errMessage = if offsetRead == input.size then 
        f"input ended while searching for $s"
       else 
         val expectedCharacter = s.charAt(offsetRead)
         val badCharacter = input.charAt(offsetRead)
         f"expected $expectedCharacter but got $badCharacter instead"
       val errList = List((Location(input, offsetRead), errMessage))
       (offsetRead, errList, Left(ParseError(errList)))

    )

    def regex(r: Regex): ParserImpl[String] =
    ParserImpl.apply(input => 
      r.findPrefixOf(input) match {
        case Some(value) => (value.size, List.empty, Right(value))
        case None => {
          val errList = List((Location(input, 0), "regex did not match"))
          (0, errList, Left(ParseError(errList)))
        }
      }
    )

    def unit[A](a: A): ParserImpl[A] =
      ParserImpl.apply { _ => (0, List.empty, Right(a))}
  
