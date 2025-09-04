import Result.Falsified
opaque type Gen[+A] = State[RNG, A]
opaque type SGen[+A] = Int => Gen[A]

opaque type MaxSize = Int

object MaxSize:
  extension (m: MaxSize) def toInt: Int = m
  def fromInt(i: Int): MaxSize = i

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
  case Proved

  def isFalsified: Boolean = this match {
    case Falsified(_, _) => true
    case _               => false
  }

  def mapFailure(f: FailedCase => FailedCase): Result =
    this match {
      case Falsified(failure, successes) => Falsified(f(failure), successes)
      case other => other 
    }

opaque type Prop = (MaxSize, TestCases, RNG) => Result
type FailedCase = String
type SuccessCount = Int

object Prop:
  extension (self: Prop)
    def &&(that: Prop): Prop =
      (maxSize, testCases, rng) =>
        self(maxSize, testCases, rng) match {
          case Result.Passed | Result.Proved => that(maxSize, testCases, rng)
          case failure       => failure
        }
  extension (self: Prop)
    def ||(that: Prop): Prop =
      (maxSize, testCases, rng) =>
        self(maxSize, testCases, rng) match {
          case Falsified(_, _) => that(maxSize, testCases, rng)
          case success         => success
        }

  extension (self: Prop)
    def tag(msg: String): Prop =
      (maxSize, testCases, rng) =>
        self(maxSize, testCases, rng).mapFailure(err => s"$msg($err)")

  extension (self: Prop)
    def run: Unit =
      self(100, 100, SimpleRNG(System.currentTimeMillis)) match {
        case Result.Falsified(failure, successes) =>
          println(s"! Falsified after $successes passed tests: \n $failure")
        case Result.Passed =>
          println(s"+ OK. passed 100 tests")
        case Result.Proved =>
          println(s"+ OK, proved property.")
      }

    def check(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis())
      ): Result = 
        self(maxSize, testCases, rng)

  def verify(p: => Boolean): Prop =
    (_, _, _) => if p then Result.Proved else Result.Falsified("{}", 0)


  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception : ${e.getMessage()}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  @annotation.targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (maxSize, testCases, rng) =>
      val casesPerSize = (testCases.toInt - 1) / maxSize.toInt + 1
      val props: LazyList[Prop] = LazyList
        .from(0)
        .take((testCases.toInt min maxSize.toInt) + 1)
        .map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng))
          .toList
          .reduce(_ && _)
      prop(maxSize, testCases, rng)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (_, testCases, rng) =>
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

  extension [A](self: Gen[A])
    def unsized: SGen[A] =
      _ => self

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
      g.flatMap(self.listOfN(_))

    def list: SGen[List[A]] = self.listOfN(_)

    def nonEmptyList: SGen[List[A]] =
      s =>
        if s == 0 then self.listOfN(1)
        else self.listOfN(s)

object SGen:
  def boolean: SGen[Boolean] = s => Gen.boolean

  def union[A](g1: SGen[A], g2: SGen[A]): SGen[A] =
    s => Gen.union(g1(s), g2(s))

  def weighted[A](g1: (SGen[A], Double), g2: (SGen[A], Double)): SGen[A] =
    s => Gen.weighted((g1._1(s), g1._2), (g2._1(s), g2._2))

  extension [A](self: SGen[A])
    def map2[B](that: SGen[B])(f: (A, B) => B): SGen[B] =
      s => self(s).map2(that(s))(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      s => self(s).flatMap(a => f(a)(s))

    def zip[B](that: SGen[B]): SGen[(A, B)] =
      s => Gen.zip(self(s))(that(s))
