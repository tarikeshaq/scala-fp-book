import MyLazyList.*
def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if cond then onTrue else onFalse

enum MyLazyList[+A]:
  case Empty
  case Cons(h: () => A, tail: () => MyLazyList[A])

  def headOption: Option[A] =
    foldRight(None)((a, _) => Some(a))

  def toList: List[A] = this match {
    case Empty         => Nil
    case Cons(h, tail) => h() :: tail().toList
  }

  def take(n: Int): MyLazyList[A] = unfold((this, n))(s =>
    s match {
      case (Cons(head, tail), n) if n > 1 => Some(head(), (tail(), n - 1))
      case (Cons(head, _), n) if n == 1   => Some(head(), (empty, n - 1))
      case _                              => None
    }
  )

  def drop(n: Int): MyLazyList[A] =
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _                      => this
    }

  def takeWhile(p: A => Boolean): MyLazyList[A] = unfold(this)(s =>
    s match {
      case Cons(h, tail) if p(h()) => Some(h(), tail())
      case _                       => None
    }
  )

  def zipAll[B](that: MyLazyList[B]): MyLazyList[(Option[A], Option[B])] =
    unfold((this, that))(_ match {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
      case (_, Cons(h1, t1)) => Some((None, Some(h1())), (empty, t1()))
      case _                 => None
    })

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
    case Empty         => acc
    case Cons(h, tail) => f(h(), tail().foldRight(acc)(f))
  }

  def foldLeft[B](acc: => B)(f: (=> B, A) => B): B = this match {
    case Empty => acc
    case Cons(h, tail) => tail().foldLeft(f(acc, h()))(f)
  }

  def map[B](f: A => B): MyLazyList[B] = unfold(this)(s =>
    s match {
      case Empty         => None
      case Cons(h, tail) => Some(f(h()), tail())
    }
  )

  def filter(p: A => Boolean): MyLazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  def append[B >: A](other: => MyLazyList[B]): MyLazyList[B] =
    foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => MyLazyList[B]): MyLazyList[B] =
    foldRight(empty)((a, b) => f(a).append(b))

  def tails: MyLazyList[MyLazyList[A]] = unfold(this)(l =>
    l match {
      case Empty         => None
      case Cons(h, tail) => Some(l, tail())
    }
  )

  def scanLeft[B](acc: => B)(f: (A, => B) => B): MyLazyList[B] =
    unfold((this, acc))((l, acc) =>
      l match {
        case Empty => None
        case Cons(h, tail) => {
          lazy val temp = f(h(), acc)
          Some(temp, (tail(), temp))
        }
      }
    )

  def scanRight[B](acc: => B)(f: (A, => B) => B): MyLazyList[B] =
    foldRight((MyLazyList(acc), acc))((e, l) =>
      lazy val temp = l
      val c = f(e, temp(1))
      lazy val res = cons(c, temp(0))
      (res, c)
    )._1

  def startsWith[B >: A](prefix: MyLazyList[B]): Boolean =
    zipAll(prefix)
      .takeWhile(_ match {
        case (_, Some(_)) => true
        case _            => false
      })
      .forAll(_ match {
        case (Some(v1), Some(v2)) => v1 == v2
        case _                    => false
      })

  def hasSubsequence[B >: A](l: MyLazyList[B]): Boolean =
    tails.exists(_.startsWith(l))

object MyLazyList:
  def cons[A](h: => A, t: => MyLazyList[A]): MyLazyList[A] =
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)

  def empty[A]: MyLazyList[A] = Empty

  def apply[A](as: A*): MyLazyList[A] =
    if as.isEmpty then Empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): MyLazyList[A] = unfold(a)(_ => Some(a, a))

  def from(n: Int): MyLazyList[Int] = unfold(n)(a => Some(a, a + 1))

  def fibs: MyLazyList[Int] = unfold((0, 1))(_ match {
    case (a, b) => Some(a, (b, a + b))
  })

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): MyLazyList[A] = f(
    state
  ) match {
    case None                    => Empty
    case Some((elem, nextState)) => cons(elem, unfold(nextState)(f))
  }
