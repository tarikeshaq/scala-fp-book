// A functor is a typeclass that defines behavor of certain
// types that are mappable
trait Functor[F[_]]:
  extension [A](as: F[A]) def map[B](f: A => B): F[B]
  extension [A, B](fab: F[(A, B)])
    def distribute: (F[A], F[B]) =
      (fab.map(_(0)), fab.map(_(1)))
  extension [A, B](fab: Either[F[A], F[B]])
    def codistribute: F[Either[A, B]] = fab match {
      case Left(fa)  => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))
    }

given Functor[MyList] with
  extension [A](as: MyList[A])
    def map[B](f: A => B): MyList[B] =
      MyList.map(as, f)

given Functor[Option] with
  extension [A](o: Option[A]) def map[B](f: A => B): Option[B] = o.map(f)

given Functor[List] with
  extension [A](as: List[A]) def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Applicative[F]:
  def unit[A](a: A): F[A]

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(f(a).map(g))
  def join[A](ffa: F[F[A]]): F[A] =
    ffa.flatMap(identity)

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]
    override def map2[B, C](other: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => other.map(b => f(a, b)))
    def void: F[Unit] = fa.map(_ => ())

    def doWhile(cond: A => F[Boolean]): F[Unit] =
      for
        a <- fa
        ok <- cond(a)
        _ <- if ok then doWhile(cond) else unit(())
      yield ()

    def forever[B]: F[B] =
      lazy val t: F[B] = fa.forever
      fa.flatMap(_ => t)

  def foldM[A, B](
      l: LazyList[A]
  )(z: B)(f: (B, A) => F[B]): F[B] =
    l match
      case h #:: t => f(z, h).flatMap(z2 => foldM(t)(z2)(f))
      case _       => unit(z)

  def foldM_[A, B](
      l: LazyList[A]
  )(z: B)(f: (B, A) => F[B]): F[Unit] =
    foldM(l)(z)(f).void

  def foreachM[A](
      l: LazyList[A]
  )(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((u, a) => f(a).void)

case class Id[A](value: A)

object Id:
  given identityMonad: Monad[Id] with
    def unit[A](a: A): Id[A] = Id(value = a)

    extension [A](id: Id[A])
      def flatMap[B](f: A => Id[B]): Id[B] =
        f(id.value)

object Monad:
  given genMonad: Monad[Gen] with
    def unit[A](a: A): Gen[A] = Gen.unit(a)

    extension [A](g: Gen[A])
      def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(g)(f)

  given stateMonad[S]: Monad[[x] =>> State[S, x]] with
    def unit[A](a: A): State[S, A] = State(s => (a, s))

    extension [A](s: State[S, A])
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(s)(f)

  given optionMonad: Monad[Option] with
    def unit[A](a: A): Option[A] = Some(a)

    extension [A](o: Option[A])
      def flatMap[B](f: A => Option[B]): Option[B] = o match {
        case Some(a) => f(a)
        case None    => None
      }

  given function0Monad: Monad[Function0] with
    def unit[A](a: A): () => A = () => a

    extension [A](fa: () => A)
      def flatMap[B](f: A => () => B): () => B =
        () => f(fa())()

  given listMonad: Monad[List] with
    def unit[A](a: A): List[A] = List(a)

    extension [A](as: List[A])
      def flatMap[B](f: A => List[B]): List[B] =
        as.flatMap(f)

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: A): LazyList[A] = LazyList(a)

    extension [A](ll: LazyList[A])
      def flatMap[B](f: A => LazyList[B]): LazyList[B] =
        ll.flatMap(f)
  given parMonad: Monad[Par] with
    def unit[A](a: A): Par[A] = Par.unit(a)

    extension [A](p: Par[A])
      def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(p)(f)
  given parserMonad: Monad[Parser] with
    def unit[A](a: A): Parser[A] = ParsersImpl.unit(a)

    extension [A](p: Parser[A])
      def flatMap[B](f: A => Parser[B]): Parser[B] =
        ParsersImpl.flatMap(p)(f)
