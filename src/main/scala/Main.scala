import Option.*
@main def hello(): Unit =
  val res = Some(1).flatMap(a => None) 
  println(res)
  val absO: Option[Double] => Option[Double] = lift(math.abs)
  println(absO(None))
