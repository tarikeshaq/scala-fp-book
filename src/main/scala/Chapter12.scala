case class Tree[+A](head: A, tail: List[Tree[A]])

object Tree:
  extension [A](t: Tree[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      val next = f(t.head, acc)
      t.tail.foldRight(next) {
        case (tNext, nextAcc) => tNext.foldRight(nextAcc)(f)
      }

trait Traverse[F[_]] extends Functor[F], Foldable[F]:
  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

    def map[B](f: A => B): F[B] =
      fa.traverse(a => Id(f(a))).value

    override def foldMap[B](f: A => B)(using m: Monoid[B]): B = 
      fa.traverse[[x] =>> Const[B, x], Nothing](f)

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverse(
        a =>
          for 
           s <- State.get[S]
           (b, s2) = f(a, s)
           _ <- State.set(s2)
          yield b
        ).run(s)

    def zipWithIndex: F[(A, Int)] =
      mapAccum(1)((a, s) => ((a, s+1), s+1))(0)

    override def toList: List[A] =
      mapAccum(Nil)((a, s) => ((), a :: s))(1).reverse 

    def reverse: F[A] =
      mapAccum(fa.toList.reverse)((a, s) => (s.head, s.tail))(0)


  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)


type Const[A, B] = A

given monoidApplicative[M](using m: Monoid[M]): Applicative[[x] =>> Const[M, x]] with
  def unit[A](a: A): M = m.empty
  
  override def apply[A, B](m1: M)(m2: M): M =
    m.combine(m1, m2)

object Traverse:
  given listTraverse: Traverse[List] with
    extension [A](fa: List[A])
     override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
       fa.foldRight(summon[Applicative[G]].unit(List.empty)) {
         case (a, gl) => f(a).map2(gl)(_ :: _)
       }


  given optionTraverse: Traverse[Option] with
    extension [A](fa: Option[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
        val g = summon[Applicative[G]]
        fa match {
          case None => g.unit(None)
          case Some(a) => f(a).map2(g.unit(None)) {
            case (b, _) => Some(b)
          }
        }

  given treeTraverse: Traverse[Tree] with
     extension [A](fa: Tree[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
       val g = summon[Applicative[G]]
       f(fa.head).map2(fa.tail.traverse(_.traverse(f)))(Tree(_, _))
       

  given mapTraverse[K]: Traverse[[x] =>> Map[K, x]] with
    extension [V](m: Map[K, V])
      override def traverse[G[_]: Applicative, B](f: V => G[B]): G[Map[K, B]] =
        val g = summon[Applicative[G]]
        m.foldRight(g.unit(Map[K, B]())) {
          case ((k, v), acc) => f(v).map2(acc) {
            case (b, innerM) => innerM + (k -> b)
          }
        }


trait Applicative[F[_]] extends Functor[F]:
  def unit[A](a: A): F[A]

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty))((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(a => a)


  def traverseM[K, V1, V2](m: Map[K, V1])(f: V1 => F[V2]): F[Map[K, V2]] =
    m.foldRight(unit(Map.empty)) {
      case ((k, v), acc) => f(v).map2(acc) {
        case (v2, innerM) => innerM + (k -> v2)
      }
    }

  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] =
    traverseM(m)(a => a)
    

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)((fn, a) => fn(a))

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List.empty)) {
      case (a, fl) => f(a).map2(fl) {
        case (b, l) if b => a :: l
        case (_, l) => l
      }
    }
  
  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
    val F = this
    val res: Applicative[[x] =>> (F[x], G[x])] = new:
      def unit[A](a: A): (F[A], G[A]) =
        (F.unit(a), G.unit(a))

      extension [A](fa: (F[A], G[A])) override def map2[B, C](fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = (fa, fb) match {
        case ((fa1, fa2), (fb1, fb2)) => (F.map2(fa1)(fb1)(f), G.map2(fa2)(fb2)(f))
      }
    res
  
  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] =
    val F = this
    new Applicative[[x] =>> F[G[x]]]:
      def unit[A](a: A): F[G[A]] = 
        F.unit(G.unit(a))

      extension [A](fa: F[G[A]]) override def map2[B, C](fb: F[G[B]])(f: (A, B) => C): F[G[C]] = 
        F.map2(fa)(fb)((a, b) => G.map2(a)(b)(f))


  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa) 

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

    def map3[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      val a = apply(unit(f.curried))(fa)
      val b = apply(a)(fb)
      apply(b)(fc)

    def map4[B, C, D, E](fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C,D) => E): F[E] =
      val a = apply(unit(f.curried))(fa)
      val b = apply(a)(fb)
      val c = apply(b)(fc) 
      apply(c)(fd)
opaque type ZipList[+A] = LazyList[A]
object ZipList:
  def fromLazyList[A](ll: LazyList[A]): ZipList[A] = ll

  extension [A](zl: ZipList[A])
    def foreach(f: A => Unit): Unit = zl.foreach(f)

  given zipListApplicative: Applicative[ZipList] with 
    def unit[A](a: A): ZipList[A] = LazyList.continually(a)
    

    extension [A](ll: ZipList[A])
      override def map2[B, C](lb: ZipList[B])(f: (A, B) => C): ZipList[C] =
        ll.zip(lb).map(f.tupled)

given eitherMonad[E]: Monad[[x] =>> Either[E, x]] with
  def unit[A](a: A): Either[E, A] =
    Right(a)

  extension [A](e: Either[E, A])
    def flatMap[B](f: A => Either[E, B]): Either[E, B] = e match {
      case Right(value) => f(value)
      case Left(e) => Left(e)
    }
enum Validated[+E, +A]:
  case Valid(get: A)
  case Invalid(error: E)

object Validated:
 given validatedApplicative[E: SemiGroup]: Applicative[[x] =>> Validated[E, x]] with
    def unit[A](a: A): Validated[E, A] =
      Valid(a)

    extension [A](v: Validated[E, A])
      override def map2[B, C](vb: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
        (v, vb) match {
          case (Valid(a), Valid(b)) => Valid(f(a, b))
          case (Invalid(e1), Invalid(e2)) => Invalid(summon[SemiGroup[E]].combine(e1, e2))
          case (Invalid(e), _) => Invalid(e)
          case (_, Invalid(e)) => Invalid(e)
        }
      

case class NonEmptyList[+A](head: A, tail: List[A]):
  def toList: List[A] = head :: tail

object NonEmptyList:
  def apply[A](head: A, tail: A*): NonEmptyList[A] =
    NonEmptyList(head, tail.toList)

  given nelSemiGroup[A]: SemiGroup[NonEmptyList[A]] with
    def combine(a1: NonEmptyList[A], a2: NonEmptyList[A]) =
      NonEmptyList(a1.head, a1.tail ++ a2.toList)

trait SemiGroup[A]:
  def combine(a1: A, a2: A): A

