def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if cond then onTrue else onFalse

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, tail: () => LazyList[A])

  def headOption: Option[A] =
    foldRight(None)((a, _) => Some(a))

  def toList: List[A] = this match {
    case Empty         => Nil
    case Cons(h, tail) => h() :: tail().toList
  }

  def take(n: Int): LazyList[A] =
    this match {
      case Cons(h, tail) if n > 1  => LazyList.cons(h(), tail().take(n - 1))
      case Cons(h, tail) if n == 1 => LazyList.cons(h(), LazyList.empty())
      case _                       => Empty
    }

  def drop(n: Int): LazyList[A] =
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _                      => this
    }

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty())((a, b) =>
      if p(a) then LazyList.cons(a, b) else LazyList.empty()
    )

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
    case Empty         => acc
    case Cons(h, tail) => f(h(), tail().foldRight(acc)(f))
  }

object LazyList:
  def cons[A](h: => A, t: => LazyList[A]): LazyList[A] =
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)

  def empty[A](): LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then Empty
    else cons(as.head, apply(as.tail*))
