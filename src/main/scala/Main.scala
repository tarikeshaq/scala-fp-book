@main def hello(): Unit =
  println(MyProg.formatTransform(10, MyProg.fib)) 
  val arr = Seq(1,2, 3).toArray
  val found = MyProg.isSorted(Array('c', 'a'), (x, y) => x > y)
  val applied = MyProg.partial[Int, Int, Int](1, (a, b) => a + b)
  val curried = MyProg.curry[Int, Int, Int]((a, b) => a + b)
  val res1 = curried(2)
  val res2 = res1(3)
  val uncurried = MyProg.uncurry(curried)
  val res3 = uncurried(2, 3)
  println(res2 == res3)
