import List.*
import Tree.*

@main def hello(): Unit =
  val l1 = List(1, 2, 3, 4)
  val l2 = List(2, 1, 4)
  val l3 = List(82, 48, 32)
  val t = Branch(Branch(Branch (Branch(Leaf(28), Leaf(0)), Leaf(50)), Leaf(1)), Leaf(1))
  val res = t.maximum
  println(res)
