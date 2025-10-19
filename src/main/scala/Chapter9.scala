trait Parsers[ParseError[+_], Parser[+_]]:
  extension [A, E](p: Parser[A])
    def run(input: String): Either[ParseError[E], A]

    infix def or(p2: Parser[A]): Parser[A]

    def |(p2: Parser[A]): Parser[A] = p.or(p2)

    def listOfN(n: Int): Parser[List[A]]

    def map[B](f: A => B): Parser[B]

    def map2[B, C](pb: Parser[B])(f: (A, B) => C): Parser[C]

    def flatMap[B](f: A => Parser[B]): Parser[B]

    // string("hello").count().run("hellohello") == Right(2)
    def count(): Parser[Int]

    def countNonZero(): Parser[Int] = p.count().flatMap(gtZero(_))

    def count2[B](pb: Parser[B]): Parser[(Int, Int)] = p.count().map2(pb.countNonZero())((_, _))

  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]
  def orString(s1: String, s2: String): Parser[String]
  def gtZero(num: Int): Parser[Int]
