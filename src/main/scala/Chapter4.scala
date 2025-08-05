import scala.util.control.NonFatal
import scala.compiletime.ops.string
enum MyOption[+A]:
  case None
  case Some(get: A)

  def map[B](f: A => B): MyOption[B] = this match {
    case None      => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](d: => B): B = this match {
    case None      => d
    case Some(get) => get
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = map(Some(_)).getOrElse(ob)

  def filter(p: A => Boolean): MyOption[A] =
    flatMap(a => if p(a) then Some(a) else None)

import MyOption.*
def mean(xs: Seq[Double]): MyEither[String, Double] =
  if xs.isEmpty then Left("Cant find mean of no elements")
  else Right(xs.sum / xs.length)

def variance(xs: Seq[Double]): MyEither[String, Double] =
  mean(xs).flatMap(c => mean(xs.map(x => math.pow(x - c, 2))))

def lift[A, B](f: A => B): MyOption[A] => MyOption[B] =
  _.map(f)

def toIntOption(s: String): MyOption[Int] =
  try Some(s.toInt)
  catch case _: NumberFormatException => None

def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
  (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _                  => None
  }

def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] = traverse(as)(a => a)

def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
  as.foldRight(Some(Nil))((e, acc) => acc.flatMap(xs => f(e).map(e => e :: xs)))

enum MyEither[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): MyEither[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(err)    => Left(err)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Left(value)  => default
    case Right(value) => value
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] =
    map(Right(_)).getOrElse(b)

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = map(f) match {
    case Right(value) => value
    case Left(value)  => Left(value)
  }

  def map2[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    (this, that) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(v), _)         => Left(v)
      case (_, Left(v))         => Left(v)
    }

def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
  as.foldRight(Right(Nil: List[B]))((e, acc) =>
    acc.map2(f(e))((xs, x) => x :: xs)
  )

def sequence[E, A](as: List[MyEither[E, A]]): MyEither[E, List[A]] =
  traverse(as)(a => a)

object MyEither {
  def catchNonFatal[A](a: => A): MyEither[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)
}

def parseInsuranceRate(age: String, num: String): MyEither[Throwable, Double] =
  for
    a <- MyEither.catchNonFatal(age.toInt)
    tickets <- MyEither.catchNonFatal(num.toInt)
  yield (a + tickets).toDouble

import MyEither.*
