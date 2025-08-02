import List.*

@main def hello(): Unit =
 val l = List(1, 2, 3, 4) 
 val res = foldRight(l, 0, _ + _) 
 val res2 = reverse(l) 
 println(res2)
