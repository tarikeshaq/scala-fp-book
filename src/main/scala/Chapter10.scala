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

def foldMapV[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  if as.isEmpty then m.empty
  else if as.length == 1 then f(as(0))
  else
    val middle = as.length / 2
    val first = foldMapV(as.slice(0, middle), m)(f)
    val second = foldMapV(as.slice(middle, as.length), m)(f)
    m.combine(first, second)


def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
  def combine(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.combine)
  def empty: Par[A] = Par.unit(m.empty)

def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
  foldMapV(as, par(m))(Par.asyncF(f))


enum WC:
  case Stub(chars: String)
  case Part(lStub: String, words: Int, rStub: String)

val wcMonoid: Monoid[WC] = new:
  // How we combine WCs:
  // If we have two parts, then we merge the parts by having the result
  // have the start of the first, end of the second and words = w1 + w2 + 1 (to accomodate for the rStub and lStub)
  // if either side is a stub, we merge the appropriate Stubs, accounting for any whitespaces that show up, turing stubs into parts when encountering those.
  // eg:
  // "lorem ipsum dolor sit amet, "
  //  Stub(lor), Part("em", 0, " "), Stub("ips"), Part("um", 0, " "),
  //  Stub("dol"), Part("or", 0, ""), Stub("sit"), Part(" ", 0, "am"),
  //  Stub("et,"), Part(" ", 0, "")
  //  ---
  //  Part("lorem", 0, " "), Part("ipsum", 0, " "), Part("dolor", 0, " ")
  //  Part("sit ", 0, "am"),
  //  Part("et, ", 0, "")
  //  ---
  //  Part("lorem", 1, " "), Part("dolor", 1, "am"), Part("et,", 0, "")
  //  ---
  //  Part("lorem", 3, "am"), Part("et,", 0, "")
  //  ---
  //  Part("lorem", 4, )
  import WC.*
  def combine(a1: WC, a2: WC): WC = (a1, a2) match {
    case (Stub(s1), Stub(s2)) => Stub(s1 ++ s2)
    case (Stub(s1), Part(s2a, n, s2b)) => Part(s1 ++ s2a, n, s2b)
    case (Part(s1a, n, s1b), Stub(s2)) => Part(s1a, n, s1b ++ s2)
    case (Part(s1a, na, s1b), Part(s2a, nb, s2b)) => {
      val count = if s1b == "" && s2a == "" then na + nb else na + nb + 1
      Part(s1a, count, s2b)
    }
  }
  def empty: WC = Stub("")


def wordCount(s: String): Par[Int]=
  parFoldMap(s.toIndexedSeq, wcMonoid) {
    case ' ' => WC.Part("", 0, "")
    case c => WC.Stub(c.toString)
  }.map {
    case WC.Stub(_) => 1
    case WC.Part(lStub, words, rStub) => (lStub, rStub) match {
      case ("", "") => words
      case ("", rStub) => words + 1
      case (lStub, "") => words + 1
      case _ => words + 2
    }
  }
