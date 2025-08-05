def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = if cond then onTrue else onFalse

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, tail: () => LazyList[A])

  def headOption: Option[A] = this match {
    case Empty => None 
    case Cons(h, _) => Some(h())
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

