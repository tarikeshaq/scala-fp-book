// Chapter 13: External effects & IO:
import scala.io.StdIn.readLine
import scala.util.Try

enum Free[F[_], A]:
  case Return(a: A)
  case Suspend(s: F[A])
  case FlatMap[F[_], A, B](s: Free[F, A], k: A => Free[F, B]) extends Free[F, B]

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] =
    this.flatMap(a => Return(f(a)))

  @annotation.tailrec
  final def step: Free[F, A] = this match {
    case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
    case FlatMap(Return(x), f)     => f(x).step
    case _                         => this
  }

  def run(using F: Monad[F]): F[A] = this.step match {
    case Return(a)  => F.unit(a)
    case Suspend(s) => s
    case FlatMap(x, f) =>
      x match {
        case Suspend(s) => s.flatMap(a => f(a).run)
        case _ =>
          sys.error("Impossible, the step function handles those branches")
      }
  }

  def runFree[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): G[A] =
    step match {
      case Return(a)              => G.unit(a)
      case Suspend(s)             => t(s)
      case FlatMap(Suspend(r), f) => t(r).flatMap(a => f(a).runFree(t))
      case _ => sys.error("Impossible, step handles these cases")
    }

  def translate[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): Free[G, A] =
    runFree([x] => (fx: F[x]) => Suspend(t(fx)))

object Free:
  given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
    def unit[A](a: A): Free[F, A] = Return(a)

    extension [A](fa: Free[F, A])
      def flatMap[B](f: A => Free[F, B]): Free[F, B] =
        fa.flatMap(f)

    extension [A](fa: Free[Function0, A])
      @annotation.tailrec
      final def runTrampoline: A = fa match {
        case Return(a)  => a
        case Suspend(s) => s()
        case FlatMap(x, f) =>
          x match {
            case Return(a)     => f(a).runTrampoline
            case Suspend(s)    => f(s()).runTrampoline
            case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).runTrampoline
          }
      }

    extension [A](fa: Free[Console, A])
      def toThunk: () => A =
        fa.runFree([x] => (c: Console[x]) => c.toThunk)

      def toPar: Par[A] =
        fa.runFree([x] => (c: Console[x]) => c.toPar)

      def unsafeRunConsole: A =
        fa.translate([x] => (c: Console[x]) => c.toThunk).runTrampoline

type TailRec[A] = Free[Function0, A]
type Async[A] = Free[Par, A]

enum Console[A]:
  case ReadLine extends Console[Option[String]]
  case PrintLine(msg: String) extends Console[Unit]

  def toPar: Par[A] = this match {
    case ReadLine       => Par.lazyUnit(Try(readLine()).toOption)
    case PrintLine(msg) => Par.lazyUnit(println(msg))
  }

  def toThunk: () => A = this match {
    case ReadLine       => () => Try(readLine()).toOption
    case PrintLine(msg) => () => println(msg)
  }

object Console:
  def readLn: Free[Console, Option[String]] =
    Free.Suspend(ReadLine)

  def printLn(line: String): Free[Console, Unit] =
    Free.Suspend(PrintLine(line))
