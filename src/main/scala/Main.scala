import List.*

@main def hello(): Unit =
  val l1 = List(1, 2, 3, 4)
  val l2 = List(2, 1, 4)
  val l3 = List(82, 48, 32)
  val res = hasSubsequence(l1, l2)
  println(res)
