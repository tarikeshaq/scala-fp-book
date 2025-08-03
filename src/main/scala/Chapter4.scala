import scala.util.control.NonFatal
import scala.compiletime.ops.string
enum Option[+A]:
  case None
  case Some(get: A)

  def map[B](f: A => B): Option[B] = this match {
    case None      => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](d: => B): B = this match {
    case None      => d
    case Some(get) => get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(p: A => Boolean): Option[A] =
    flatMap(a => if p(a) then Some(a) else None)

import Option.*
def mean(xs: Seq[Double]): Either[String, Double] =
  if xs.isEmpty then Left("Cant find mean of no elements")
  else Right(xs.sum / xs.length)

def variance(xs: Seq[Double]): Either[String, Double] =
  mean(xs).flatMap(c => mean(xs.map(x => math.pow(x - c, 2))))

def lift[A, B](f: A => B): Option[A] => Option[B] =
  _.map(f)

def toIntOption(s: String): Option[Int] =
  try Some(s.toInt)
  catch case _: NumberFormatException => None

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _                  => None
  }

def sequence[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(a => a)

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldRight(Some(Nil))((e, acc) => acc.flatMap(xs => f(e).map(e => e :: xs)))

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(err)    => Left(err)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Left(value)  => default
    case Right(value) => value
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    map(Right(_)).getOrElse(b)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = map(f) match {
    case Right(value) => value
    case Left(value)  => Left(value)
  }

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, that) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(v), _)         => Left(v)
      case (_, Left(v))         => Left(v)
    }

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  as.foldRight(Right(Nil: List[B]))((e, acc) =>
    acc.map2(f(e))((xs, x) => x :: xs)
  )

def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
  traverse(as)(a => a)

object Either {
  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)
}

def parseInsuranceRate(age: String, num: String): Either[Throwable, Double] =
  for
    a <- Either.catchNonFatal(age.toInt)
    tickets <- Either.catchNonFatal(num.toInt)
  yield (a + tickets).toDouble

import Either.*
