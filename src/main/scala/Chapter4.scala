enum Option[+A]:
 case None
 case Some(get: A)

 def map[B](f: A => B): Option[B] = this match {
   case None => None
   case Some(get) => Some(f(get))
 }

 def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None) 
 

 def getOrElse[B >: A](d: => B): B = this match {
   case None => d 
   case Some(get) => get
 }

 def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob) 

 def filter(p: A => Boolean): Option[A] = flatMap(a => if p(a) then Some(a) else None)


import Option.*
def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(c => mean(xs.map(x => math.pow(x - c, 2))))

def lift[A, B](f: A => B): Option[A] => Option[B] =
 _.map(f) 


def toIntOption(s: String): Option[Int] =
  try Some(s.toInt)
  catch case _: NumberFormatException => None

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  case (Some(x), Some(y)) => Some(f(x, y))
  case _ => None
}

def sequence[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(a => a) 

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as.foldRight(Some(Nil))((e, acc) => 
    acc.flatMap(xs => f(e).map(e => e :: xs)))
