import MyOption.*
@main def hello(): Unit =
  val res = Some(1).flatMap(a => None)
  println(res)
  val absO: MyOption[Double] => MyOption[Double] = lift(math.abs)
  println(absO(None))
