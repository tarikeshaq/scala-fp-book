trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

val stringMonoid: Monoid[String] = new:
  def combine(a1: String, a2: String): String = a1+a2
  def empty: String = "" 
def listMonoid[A]: Monoid[List[A]] = new:
  def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  def empty: List[A] = Nil 

val intAddition: Monoid[Int] = new:
  def combine(a1: Int, a2: Int): Int = a1 + a2
  def empty: Int = 0

val intMultiplication: Monoid[Int] = new:
  def combine(a1: Int, a2: Int): Int = a1 * a2
  def empty: Int = 1

val booleanOr: Monoid[Boolean] = new:
  def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  def empty: Boolean = false

val booleanAnd: Monoid[Boolean] = new:
  def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  def empty: Boolean = true 

def optionMonoid[A]: Monoid[Option[A]] = new:
  def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
    case None => a2
    case _ => a1
  }
  def empty: Option[A] = None 

def endoMonoid[A]: Monoid[A => A] = new:
  def combine(a1: A => A, a2: A => A): A => A =
    a => a2(a1(a))
  def empty: A => A = a => a
object MonoidLaws:
  def identity[A](m: Monoid[A], gen: Gen[A]): Prop =
   // There are two laws with Monoids:
   // - combining has an identity: i.e: empty value with anything returns the original
   // - combining is associative (i.e: comb(comb(a, b), c) == comb(a, comb(b, c)))
   Prop.forAll(gen) {
     a => 
       val emp = m.empty
       m.combine(a, emp) == a &&
       m.combine(emp, a) == a
   }

  def associativity[A](m: Monoid[A], gen: Gen[(A, A, A)]): Prop =
     Prop.forAll(gen) {
       case (a, b, c) => 
         m.combine(m.combine(a, b), c) ==
           m.combine(a, m.combine(b, c))
     }

def combineAll[A](l: List[A], m: Monoid[A]): A =
  l.foldLeft(m.empty)(m.combine)

def foldMap[A, B](l: List[A], m: Monoid[B])(f: A => B): B =
  l.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))


def myFoldRight[A, B](l: List[A])(acc: B)(f: (A, B) => B): B =
  foldMap(l, endoMonoid[B])(f.curried)(acc)

def myFoldLeft[A, B](l: List[A])(acc: B)(f: (A, B) => B): B =
  foldMap(l, dual(endoMonoid[B]))(f.curried)(acc)

def dual[A](m: Monoid[A]): Monoid[A] =
  val res: Monoid[A] = new:
    def combine(a1: A, a2: A): A = m.combine(a2, a1)
    def empty: A = m.empty

  res



