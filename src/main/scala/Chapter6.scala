opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => 
        val (a, nextState) = underlying(s)
        f(a)(nextState)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => other.map(b => f(a, b)))

  def apply[S, A](f: S => (A, S)): State[S, A] =
    f
  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight(State.unit(Nil: List[A]))((a, b) => a.map2(b)(_ :: _))


  def get[S]: State[S, S] = s => (s, s)

  def set[S](newState: S): State[S, Unit] =
    _ => ((), newState) 

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()
  

type Rand[A] = State[RNG, A]

val int: Rand[Int] = State.apply(rng => rng.nextInt)

trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

object RNG:
  def nonNegativeInt: Rand[Int] =
    int.map(n =>
      n match {
        case v if v == Int.MinValue =>
          0 // Too lazy to do the right math here, scew!
        case v if v < 0 => v * -1
        case v          => v
      },

    )
  def nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - (i % 2))
  def double: Rand[Double] =
    nonNegativeInt.map(n => (n.toDouble / Int.MaxValue.toDouble))

  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt: Rand[(Double, Int)] =
    both(double, int)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_, _))
  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(State.unit(Nil: List[A]))((a, b) => a.map2(b)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap(i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then State.unit(mod)
      else nonNegativeLessThan(n)
    )
