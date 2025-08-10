import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.Callable
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch

opaque type Future[+A] = (A => Unit) => Unit
opaque type Par[+A] = ExecutorService => Future[A]

object Par:
  def unit[A](a: A): Par[A] =
    e => callback => callback(a)
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call(): Unit = r })

  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => cb => ppa(es) { pa => pa(es)(cb) }

  def fork[A](pa: => Par[A]): Par[A] =
    es => callback => eval(es)(pa(es)(callback))
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    n.flatMap(choices(_))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    cond.flatMap(if _ then t else f)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    key.flatMap(choices(_))

  extension [A](pa: Par[A])
    def run(e: ExecutorService): A =
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      pa(e) { a =>
        ref.set(a); latch.countDown
      }
      latch.await
      ref.get

  extension [A](pa: Par[A])
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        cb =>
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es):
            case Left(a) =>
              br match {
                case Some(b) => cb(f(a, b))
                case None    => ar = Some(a)
              }
            case Right(b) =>
              ar match {
                case Some(a) => cb(f(a, b))
                case None    => br = Some(b)
              }
          pa(es) { a => combiner ! Left(a) }
          b(es) { b => combiner ! Right(b) }

    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))
    def map3[B, C, D](b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      val ab = pa.map2(b)((a, b) => (a, b))
      ab.map2(c)((i1, c) =>
        i1 match {
          case (a, b) => f(a, b, c)
        }
      )
    def map4[B, C, D, E](b: Par[B], c: Par[C], d: Par[D])(
        f: (A, B, C, D) => E
    ): Par[E] =
      val ab = pa.map2(b)((a, b) => (a, b))
      val cd = c.map2(d)((c, d) => (c, d))
      ab.map2(cd)((ab, cd) =>
        (ab, cd) match {
          case ((a, b), (c, d)) => f(a, b, c, d)
        }
      )
    def map5[B, C, D, E, F](b: Par[B], c: Par[C], d: Par[D], e: Par[E])(
        f: (A, B, C, D, E) => F
    ): Par[F] =
      val abc = pa.map3(b, c)((a, b, c) => (a, b, c))
      abc.map3(d, e)((l1, d, e) =>
        l1 match {
          case (a, b, c) => f(a, b, c, d, e)
        }
      )

    def flatMap[B](f: A => Par[B]): Par[B] =
      join(pa.map(f))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if ps.isEmpty then unit(IndexedSeq.empty)
    else if ps.size == 1 then ps.head.map(a => IndexedSeq(a))
    else
      val (l, r) = ps.splitAt(ps.size / 2)
      val sequencedRight = sequenceBalanced(r)
      sequenceBalanced(l).map2(sequencedRight)(_ ++ _)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(ps.map(asyncF(f))))

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      val pars = ps.map(asyncF(a => if f(a) then List(a) else Nil))
      sequence(pars).map(_.flatten)

  def parReduce[A, B](
      ps: IndexedSeq[A]
  )(mapper: Option[A] => B)(join: (B, B) => B): Par[B] =
    fork:
      if ps.isEmpty then unit(mapper(None))
      else if ps.size == 1 then unit(mapper(Some(ps.head)))
      else
        val (l, r) = ps.splitAt(ps.size / 2)
        val resRight = parReduce(r)(mapper)(join)
        parReduce(l)(mapper)(join).map2(resRight)(join)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def delay[A](p: => Par[A]): Par[A] =
    e => p(e)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

def numMapper(o: Option[Int]): Int = o match {
  case None    => 0
  case Some(v) => v
}

def intParReducer: (IndexedSeq[Int], (Int, Int) => Int) => Par[Int] =
  (ints, join) => Par.parReduce(ints)(numMapper)(join)

def parSum(ls: IndexedSeq[Int]): Par[Int] =
  intParReducer(ls, _ + _)

def parMax(ls: IndexedSeq[Int]): Par[Int] =
  intParReducer(ls, Math.max)

def numWords(paragraphs: List[String]): Par[Int] =
  Par.parReduce(paragraphs.toIndexedSeq)(_ match {
    case None            => 0
    case Some(paragraph) => paragraph.split(" ").length
  })(_ + _)
