import java.util.concurrent.Executors

@main def hello(): Unit =
  import ZipList.*
  val l = fromLazyList(LazyList(1, 2, 3, 4, 5))
  val l2 = fromLazyList(LazyList(6, 7, 8, 9, 10))
  val l3 = fromLazyList(LazyList(11, 12, 13))
  val lis = List(l, l2, l3)
  val res = summon[Applicative[ZipList]].sequence(lis)
  res.foreach(println(_))
