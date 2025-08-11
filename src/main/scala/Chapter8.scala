import Result.Falsified
opaque type Gen[+A] = State[RNG, A]
opaque type TestCases = Int

object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(i: Int): TestCases = i

enum Result:
  case Passed
  case Falsified(
      failure: FailedCase,
      successes: SuccessCount
  )

  def isFalsified: Boolean = this match {
    case Falsified(_, _) => true
    case _               => false
  }

  def mapFailure(f: FailedCase => FailedCase): Result =
    this match {
      case Passed                        => Passed
      case Falsified(failure, successes) => Falsified(f(failure), successes)
    }

opaque type Prop = (TestCases, RNG) => Result
type FailedCase = String
type SuccessCount = Int

object Prop:
  extension (self: Prop)
    def &&(that: Prop): Prop =
      (testCases, rng) =>
        self.tag("and-left")(testCases, rng) match {
          case Result.Passed => that.tag("and-right")(testCases, rng)
          case failure       => failure
        }
  extension (self: Prop)
    def ||(that: Prop): Prop =
      (testCases, rng) =>
        self.tag("or-left")(testCases, rng) match {
          case Falsified(_, _) => that.tag("or-righ")(testCases, rng)
          case success         => success
        }

  extension (self: Prop)
    def tag(msg: String): Prop =
      (testCases, rng) => self(testCases, rng).mapFailure(err => s"$msg($err)")

  extension (self: Prop)
    def run: Unit =
      self(100, SimpleRNG(System.currentTimeMillis)) match {
        case Result.Falsified(failure, successes) =>
          println(s"! Falsified after $successes passed tests: \n $failure")
        case Result.Passed =>
          println(s"+ OK. passed 100 tests")
      }

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception : ${e.getMessage()}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"



  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (testCases, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(testCases)
        .map:
          case (a, i) =>
            try
              if f(a) then Result.Passed
              else Result.Falsified(a.toString, i)
            catch
              case e: Exception =>
                Result.Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Result.Passed)
object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    RNG.nonNegativeInt.map(num => {
      val m = stopExclusive - start
      val adjusted = num % m
      adjusted + start
    })

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    int.map(b => if b >= 0 then true else false)

  def pair(start: Int, stopExlusive: Int): Gen[(Int, Int)] =
    val gen = () => choose(start, stopExlusive)
    gen().map2(gen())((_, _))

  def option[A](a: Gen[A]): Gen[Option[A]] =
    a.map(Some(_))

  def unwrapOption[A](a: Gen[Option[A]])(default: => A): Gen[A] =
    a.map(_ match {
      case None    => default
      case Some(v) => v
    })

  def stringOfN(n: Int): Gen[String] =
    RNG.char.listOfN(n).map(_.mkString)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1(1).abs / (g1(1).abs + g2(1).abs)
    RNG.double.flatMap(d => if d <= g1Threshold then g1(0) else g2(0))

  extension [A](self: Gen[A])

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)
    def zip[B](that: Gen[B]): Gen[(A, B)] =
      self.map2(that)((_, _))

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

    def listOfN(g: Gen[Int]): Gen[List[A]] =
      g.flatMap(n => self.listOfN(n))
