@main def hello(): Unit =
  val l = LazyList(0, 1, 2, 3, 4)
  val l2 = LazyList.from(0).startsWith(l) 
  println(l2)
