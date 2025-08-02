import List.*

@main def hello(): Unit =
 val res = List(1, 2, 3, 4, 5) match {
   case Cons(x, Cons(2, Cons(4, _))) => x
   case Nil => 42
   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
   case _ => 101
 } 
 println(res)
