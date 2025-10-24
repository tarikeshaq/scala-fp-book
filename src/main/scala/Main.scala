@main def hello(): Unit =
  val words = List("Hello", "good bye", "wow")
  val res = myFoldLeft(words)("")(_ ++ _)
  println(res)

