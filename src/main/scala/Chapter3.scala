enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def fill[A](n: Int)(a: A): List[A] =
    if n == 0 then Nil
    else Cons(a, fill(n - 1)(a))

  def sum(ints: List[Int]): Int =
    foldRight(ints, 0, _ + _)

  def product(doubles: List[Double]): Double =
    foldRight(doubles, 1, _ * _)

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil              => sys.error("Cant get tail of empty list")
      case Cons(head, tail) => tail
    }

  def setHead[A](as: List[A], newHead: A): List[A] =
    as match {
      case Nil              => Cons(newHead, Nil)
      case Cons(head, tail) => Cons(newHead, tail)
    }

  def drop[A](as: List[A], n: Int): List[A] =
    if n == 0 then as
    else
      as match {
        case Nil         => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }

  def dropWhile[A](as: List[A], p: A => Boolean): List[A] =
    as match {
      case Nil              => Nil
      case Cons(head, tail) => if p(head) then dropWhile(tail, p) else as
    }

  def append[A](l1: List[A], l2: List[A]): List[A] =
    foldRight2(l1, l2, Cons(_, _))

  def reverse[A](as: List[A]): List[A] =
    foldRight(as, Nil, Cons(_, _))

  def init[A](as: List[A]): List[A] =
    def init_inner(as: List[A], acc: List[A]): List[A] =
      as match {
        case Nil          => Nil
        case Cons(x, Nil) => reverse(acc)
        case Cons(x, xs)  => init_inner(xs, Cons(x, acc))
      }
    init_inner(as, Nil)

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match {
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))
    }

  def foldRight2[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft[A, B => B](as, temp => temp, (a, b) => temp => b(f(a, temp)))(acc)

  def foldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match {
      case Nil         => acc
      case Cons(x, xs) => foldLeft(xs, f(x, acc), f)
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)
  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A], append)

  def incBy1(as: List[Int]): List[Int] =
    map(as, _ + 1)

  def dubToString(as: List[Double]): List[String] =
    map(as, _.toString)

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B], (e, l) => Cons(f(e), l))

  def filter[A](as: List[A], p: A => Boolean): List[A] =
    flatMap(as, a => if p(a) then List(a) else Nil)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (e, l) => append(f(e), l))

  def join[A](l1: List[A], l2: List[A], f: (A, A) => A): List[A] =
    (l1, l2) match {
      case (Nil, Nil)                     => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), join(xs1, xs2, f))
      case (l, Nil)                       => l
      case (Nil, l)                       => l
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    def matches[A](sup: List[A], sub: List[A]): Boolean =
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

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(right: Tree[A], left: Tree[A])

  def size: Int = fold(_ => 1, _ + _ + 1)

  def depth: Int = fold(_ => 1, _.max(_) + 1)

  def map[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))

  def fold[B](f: A => B, g: (B, B) => B): B = this match {
    case Leaf(value)         => f(value)
    case Branch(right, left) => g(right.fold(f, g), left.fold(f, g))
  }

object Tree:
  def apply[A](value: A): Tree[A] =
    Leaf(value)

  extension (t: Tree[Int])
    def firstPositive: Int = t match {
      case Leaf(value) => value
      case Branch(right, left) =>
        val lpos = left.firstPositive
        if lpos > 0 then lpos else right.firstPositive
    }

  extension (t: Tree[Int])
    def maximum: Int =
      t.fold(a => a, _.max(_))
