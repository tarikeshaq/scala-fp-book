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
 
