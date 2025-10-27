import Monoid.endoMonoid
trait Monoid[A] extends SemiGroup[A]:
  def empty: A

object Monoid:
  import WC.*
  given stringMonoid: Monoid[String] with
    def combine(a1: String, a2: String): String = a1 + a2
    def empty: String = ""

  given listMonoid[A]: Monoid[List[A]] with
    def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def empty: List[A] = Nil

  given intAddition: Monoid[Int] with
    def combine(a1: Int, a2: Int): Int = a1 + a2
    def empty: Int = 0

  given booleanOr: Monoid[Boolean] with
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def empty: Boolean = false

  given booleanAnd: Monoid[Boolean] with
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def empty: Boolean = true

  given optionMonoid[A]: Monoid[Option[A]] with
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
      case None => a2
      case _    => a1
    }
    def empty: Option[A] = None

  given endoMonoid[A]: Monoid[A => A] with
    def combine(a1: A => A, a2: A => A): A => A =
      a => a2(a1(a))
    def empty: A => A = a => a
  given wcMonoid: Monoid[WC] with
    def combine(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2))          => Stub(s1 ++ s2)
      case (Stub(s1), Part(s2a, n, s2b)) => Part(s1 ++ s2a, n, s2b)
      case (Part(s1a, n, s1b), Stub(s2)) => Part(s1a, n, s1b ++ s2)
      case (Part(s1a, na, s1b), Part(s2a, nb, s2b)) => {
        val count = if s1b == "" && s2a == "" then na + nb else na + nb + 1
        Part(s1a, count, s2b)
      }
    }
    def empty: WC = Stub("")

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)]
  with
    def combine(a1: (A, B), a2: (A, B)): (A, B) =
      (ma.combine(a1._1, a2._1), mb.combine(a1._2, a2._2))
    def empty: (A, B) = (ma.empty, mb.empty)

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(
          k,
          mv.combine(a1.getOrElse(k, mv.empty), a2.getOrElse(k, mv.empty))
        )

    def empty: Map[K, V] = Map()

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(a1: A => B, a2: A => B): A => B =
      a => mb.combine(a1(a), a2(a))
    def empty: A => B =
      a => mb.empty

object MonoidLaws:
  def identity[A](m: Monoid[A], gen: Gen[A]): Prop =
    // There are two laws with Monoids:
    // - combining has an identity: i.e: empty value with anything returns the original
    // - combining is associative (i.e: comb(comb(a, b), c) == comb(a, comb(b, c)))
    Prop.forAll(gen) { a =>
      val emp = m.empty
      m.combine(a, emp) == a &&
      m.combine(emp, a) == a
    }

  def associativity[A](m: Monoid[A], gen: Gen[(A, A, A)]): Prop =
    Prop.forAll(gen) { case (a, b, c) =>
      m.combine(m.combine(a, b), c) ==
        m.combine(a, m.combine(b, c))
    }

def combineAll[A](l: List[A])(using m: Monoid[A]): A =
  l.foldLeft(m.empty)(m.combine)

def foldMap[A, B](l: List[A])(f: A => B)(using m: Monoid[B]): B =
  l.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

def dual[A](using m: Monoid[A]): Monoid[A] =
  val res: Monoid[A] = new:
    def combine(a1: A, a2: A): A = m.combine(a2, a1)
    def empty: A = m.empty
  res

def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
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

def parFoldMap[A, B](as: IndexedSeq[A])(f: A => B)(using m: Monoid[B]): Par[B] =
  foldMapV(as, par(m))(Par.asyncF(f))

enum WC:
  case Stub(chars: String)
  case Part(lStub: String, words: Int, rStub: String)

def wordCount(s: String): Par[Int] =
  parFoldMap(s.toIndexedSeq) {
    case ' ' => WC.Part("", 0, "")
    case c   => WC.Stub(c.toString)
  }.map {
    case WC.Stub(_) => 1
    case WC.Part(lStub, words, rStub) =>
      (lStub, rStub) match {
        case ("", "")    => words
        case ("", rStub) => words + 1
        case (lStub, "") => words + 1
        case _           => words + 2
      }
  }

trait Foldable[F[_]]:
  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
     as.foldMap(f.curried)(using dual)(acc)
    def foldLeft[B](acc: B)(f: (B, A) => B): B = 
      as.foldMap(a => b => f(b, a))(using endoMonoid[B])(acc)
    def foldMap[B](f: A => B)(using m: Monoid[B]): B
    def combineAll(using m: Monoid[A]): A =
      as.foldLeft(m.empty)(m.combine)
    def toList: List[A] =
      as.foldMap(a => List(a))

given Foldable[MyList] with
  extension [A](as: MyList[A])
    override def foldRight[B](acc: B)(f: (A, B) => B): B =
      MyList.foldRight(as, acc, f)
    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      MyList.foldLeft(as, acc, (a, b) => f(b, a))
    def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

given Foldable[IndexedSeq] with
  extension [A](as: IndexedSeq[A])
    override def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldRight(acc)(f)
    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldLeft(acc)(f)
    def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

given Foldable[LazyList] with
  extension [A](as: LazyList[A])
    override def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldRight(acc)(f)
    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldLeft(acc)(f)
    def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

given Foldable[MyTree] with
  extension [A](t: MyTree[A])
    def foldMap[B](f: A => B)(using m: Monoid[B]): B = t match {
      case MyTree.Leaf(value) => f(value)
      case MyTree.Branch(right, left) =>
        m.combine(left.foldMap(f), right.foldMap(f))
    }
    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      def h(a: A, b: B): B =
        f(b, a)
      t.foldMap(h.curried)(acc)
    override def foldRight[B](acc: B)(f: (A, B) => B): B =
      t.foldMap(f.curried)(using dual)(acc)

given Foldable[Option] with
  extension [A](o: Option[A])
    override def foldLeft[B](acc: B)(f: (B, A) => B): B = o match {
      case None        => acc
      case Some(value) => f(acc, value)
    }
    override def foldRight[B](acc: B)(f: (A, B) => B): B =
      o.foldLeft(acc)((b, a) => f(a, b))
    def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      o.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

def bag[A](as: IndexedSeq[A]): Map[A, Int] =
  as.foldMap(a => Map(a -> 1))
