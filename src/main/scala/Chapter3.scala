enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList:
  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def fill[A](n: Int)(a: A): MyList[A] =
    if n == 0 then Nil
    else Cons(a, fill(n - 1)(a))

  def sum(ints: MyList[Int]): Int =
    foldRight(ints, 0, _ + _)

  def product(doubles: MyList[Double]): Double =
    foldRight(doubles, 1, _ * _)

  def tail[A](as: MyList[A]): MyList[A] =
    as match {
      case Nil              => sys.error("Cant get tail of empty list")
      case Cons(head, tail) => tail
    }

  def setHead[A](as: MyList[A], newHead: A): MyList[A] =
    as match {
      case Nil              => Cons(newHead, Nil)
      case Cons(head, tail) => Cons(newHead, tail)
    }

  def drop[A](as: MyList[A], n: Int): MyList[A] =
    if n == 0 then as
    else
      as match {
        case Nil         => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }

  def dropWhile[A](as: MyList[A], p: A => Boolean): MyList[A] =
    as match {
      case Nil              => Nil
      case Cons(head, tail) => if p(head) then dropWhile(tail, p) else as
    }

  def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] =
    foldRight2(l1, l2, Cons(_, _))

  def reverse[A](as: MyList[A]): MyList[A] =
    foldRight(as, Nil, Cons(_, _))

  def init[A](as: MyList[A]): MyList[A] =
    def init_inner(as: MyList[A], acc: MyList[A]): MyList[A] =
      as match {
        case Nil          => Nil
        case Cons(x, Nil) => reverse(acc)
        case Cons(x, xs)  => init_inner(xs, Cons(x, acc))
      }
    init_inner(as, Nil)

  def foldRight[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    as match {
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))
    }

  def foldRight2[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    foldLeft[A, B => B](as, temp => temp, (a, b) => temp => b(f(a, temp)))(acc)

  def foldLeft[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    as match {
      case Nil         => acc
      case Cons(x, xs) => foldLeft(xs, f(x, acc), f)
    }

  def length[A](as: MyList[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)
  def concat[A](as: MyList[MyList[A]]): MyList[A] =
    foldRight(as, Nil: MyList[A], append)

  def incBy1(as: MyList[Int]): MyList[Int] =
    map(as, _ + 1)

  def dubToString(as: MyList[Double]): MyList[String] =
    map(as, _.toString)

  def map[A, B](as: MyList[A], f: A => B): MyList[B] =
    foldRight(as, Nil: MyList[B], (e, l) => Cons(f(e), l))

  def filter[A](as: MyList[A], p: A => Boolean): MyList[A] =
    flatMap(as, a => if p(a) then MyList(a) else Nil)

  def flatMap[A, B](as: MyList[A], f: A => MyList[B]): MyList[B] =
    foldRight(as, Nil: MyList[B], (e, l) => append(f(e), l))

  def join[A](l1: MyList[A], l2: MyList[A], f: (A, A) => A): MyList[A] =
    (l1, l2) match {
      case (Nil, Nil)                     => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), join(xs1, xs2, f))
      case (l, Nil)                       => l
      case (Nil, l)                       => l
    }

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean =
    def matches[A](sup: MyList[A], sub: MyList[A]): Boolean =
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, xs), Cons(y, ys)) =>
          if x == y then matches(xs, ys) else false
      }
    sup match {
      case Nil         => if sub == Nil then true else false
      case Cons(x, xs) => matches(sup, sub) || hasSubsequence(xs, sub)
    }

enum MyTree[+A]:
  case Leaf(value: A)
  case Branch(right: MyTree[A], left: MyTree[A])

  def size: Int = fold(_ => 1, _ + _ + 1)

  def depth: Int = fold(_ => 1, _.max(_) + 1)

  def map[B](f: A => B): MyTree[B] = fold(a => Leaf(f(a)), Branch(_, _))

  def fold[B](f: A => B, g: (B, B) => B): B = this match {
    case Leaf(value)         => f(value)
    case Branch(right, left) => g(right.fold(f, g), left.fold(f, g))
  }

object MyTree:
  def apply[A](value: A): MyTree[A] =
    Leaf(value)

  extension (t: MyTree[Int])
    def firstPositive: Int = t match {
      case Leaf(value) => value
      case Branch(right, left) =>
        val lpos = left.firstPositive
        if lpos > 0 then lpos else right.firstPositive
    }

  extension (t: MyTree[Int])
    def maximum: Int =
      t.fold(a => a, _.max(_))
