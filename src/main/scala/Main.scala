@main def hello(): Unit =
  val l3 = MyLazyList(1, 2, 3).scanRight(0)(_ + _).toList 
  println(l3)
