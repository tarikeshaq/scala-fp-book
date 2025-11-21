// Chapter 13: External effects & IO:
import scala.io.StdIn.readLine

trait IO[A]:
  self =>
   def unsafeRun: A
  
   def map[B](f: A => B): IO[B] = new:
    def unsafeRun: B = 
      f(self.unsafeRun)

   def flatMap[B](f: A => IO[B]): IO[B] = new:
    def unsafeRun: B = 
      f(self.unsafeRun).unsafeRun

object IO:
  def apply[A](a: => A): IO[A] = new:
    def unsafeRun: A = a

  given monad: Monad[IO] with
    def unit[A](a: A): IO[A] = IO(a)

    extension [A](fa: IO[A]) def flatMap[B](f: A => IO[B]): IO[B] = 
      fa.flatMap(f)

def PrintLine(msg: String): IO[Unit] = 
  IO(println(msg))


def ReadLine(): IO[String] = IO(readLine())

def fahrenheitToCelsius(f: Double): Double =
  (f - 32) * 5.0/9.0


def converter: IO[Unit] = for
  _ <- PrintLine("Enter a tempreture in F: ")
  d <- ReadLine().map(_.toDouble)
  _ <- PrintLine(fahrenheitToCelsius(d).toString)
yield ()

