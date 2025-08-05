@main def hello(): Unit =
 val lazyList = LazyList(1, 2, 3, 4, 5, 6)
 val l = lazyList.takeWhile(_ < 5).drop(1).headOption
 println(l)
