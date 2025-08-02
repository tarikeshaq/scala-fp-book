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
      case Nil => Nil
      case Cons(head, tail) => if p(head) then dropWhile(tail, p) else as
    }

  def append[A](l1: List[A], l2: List[A]): List[A] =
    l1 match {
      case Nil => l2
      case Cons(head, tail) => Cons(head, append(tail, l2))
    }

  def reverse[A](as: List[A]): List[A] =
    foldRight(as, Nil, Cons(_, _)) 

  def init[A](as: List[A]): List[A] =
    def init_inner(as: List[A], acc: List[A]): List[A] =
      as match {
        case Nil => Nil
        case Cons(x, Nil) => reverse(acc) 
        case Cons(x, xs) => init_inner(xs, Cons(x, acc))
      }
    init_inner(as, Nil)

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match {
     case Nil => acc
     case Cons(x, xs) => f(x, foldRight(xs, acc, f)) 
    }

  def foldRight2[A, B](as: List[A], acc: B, f: (A, B) => B): B =
     foldLeft[A, B => B](as, temp => temp, (a, b) => temp => b(f(a, temp)))(acc)

  def foldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(x, acc), f)
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)
