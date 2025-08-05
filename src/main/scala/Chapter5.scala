import LazyList.*
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

  def take(n: Int): LazyList[A] = unfold((this, n))(s => s match {
    case (Cons(head, tail), n) if n > 1 => Some(head(), (tail(), n-1))
    case (Cons(head, _), n) if n == 1 => Some(head(), (empty, n-1))
    case _ => None
  })

  def drop(n: Int): LazyList[A] =
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _                      => this
    }

  def takeWhile(p: A => Boolean): LazyList[A] = unfold(this)(s => s match {
    case Cons(h, tail) if p(h()) => Some(h(), tail())
    case _ => None
  })

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that))(_ match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
    case (_, Cons(h1, t1)) => Some((None, Some(h1())), (empty, t1()))
    case _ => None
  })

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
    case Empty         => acc
    case Cons(h, tail) => f(h(), tail().foldRight(acc)(f))
  }

  def map[B](f: A => B): LazyList[B] = unfold(this)(s => s match {
    case Empty => None
    case Cons(h, tail) => Some(f(h()), tail())
  }) 

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  def append[B >: A](other: => LazyList[B]): LazyList[B] =
    foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, b) => f(a).append(b))

  def startsWith[B >: A](prefix: LazyList[B]): Boolean =
    zipAll(prefix)
    .takeWhile(_ match {
      case (_, Some(_)) => true
      case _ => false
    })
    .forAll(_ match {
      case (Some(v1), Some(v2)) => v1 == v2
      case _ => false 
    })
      

object LazyList:
  def cons[A](h: => A, t: => LazyList[A]): LazyList[A] =
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then Empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): LazyList[A] = unfold(a)(_ => Some(a, a))

  def from(n: Int): LazyList[Int] = unfold(n)(a => Some(a, a+1))

  def fibs: LazyList[Int] = unfold((0, 1))(_ match {
    case (a, b) => Some(a, (b, a+b))
  })

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match {
    case None => Empty
    case Some((elem, nextState)) => cons(elem, unfold(nextState)(f))
  }
