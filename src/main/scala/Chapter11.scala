// A functor is a typeclass that defines behavor of certain
// types that are mappable
trait Functor[F[_]]:
  extension [A](as: F[A])
    def map[B](f: A => B): F[B]
  extension [A, B](fab: F[(A, B)])
    def distribute: (F[A], F[B]) =
      (fab.map(_(0)), fab.map(_(1)))
  extension [A, B](fab: Either[F[A], F[B]])
    def codistribute: F[Either[A, B]] = fab match {
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))
    }

given Functor[MyList] with
    extension [A](as: MyList[A]) def map[B](f: A => B): MyList[B] = 
      MyList.map(as, f)

given Functor[Option] with
    extension [A](o: Option[A])
      def map[B](f: A => B): Option[B] = o.map(f)
  
given Functor[List] with
    extension [A](as: List[A])
      def map[B](f: A => B): List[B] = as.map(f)
 

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: A): F[A]

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty)) {
      case (a, fl) => f(a).map2(fl)(_ :: _)
    }

  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(a => a)

  extension [A](as: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] =
      as.flatMap(a => unit(f(a)))

    def map2[B, C](other: F[B])(f: (A, B) => C): F[C] =
      as.flatMap(a => other.map(b => f(a, b)))


object Monad:
  given genMonad: Monad[Gen] with
    def unit[A](a: A): Gen[A] = Gen.unit(a)

    extension [A](g: Gen[A])
     def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen.flatMap(g)(f) 

  given optionMonad: Monad[Option] with
    def unit[A](a: A): Option[A] = Some(a)

    extension [A](o: Option[A])
      def flatMap[B](f: A => Option[B]): Option[B] = o match {
        case Some(a) => f(a)
        case None => None
      } 
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



  
  

