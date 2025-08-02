enum List[+A]:
 case Nil
 case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*)) 
  def sum(ints: List[Int]): Int =
    def sum_inner(ints: List[Int], acc: Int): Int =
      ints match {
        case Nil => acc
        case Cons(head, tail) => sum_inner(tail, acc + head)
      }
    sum_inner(ints, 0)
  def product(doubles: List[Double]): Double =
    def product_inner(doubles: List[Double], acc: Double): Double =
      doubles match {
        case Nil => acc
        case Cons(head, tail) => product_inner(tail, acc * head)
      }
    product_inner(doubles, 1)
