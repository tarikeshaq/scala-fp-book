object MyProg:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n
  def factorial(x: Int) =
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, acc * n)
    go(x, 1)
  def fib(x: Int) =
    def go(x: Int, acc1: Int, acc2: Int): Int =
      if x == 0 then acc1
      else if x == 1 then acc2
      else go(x - 1, acc2, acc1 + acc2)
    go(x, 0, 1)

  def formatTransform(n: Int, transform: Int => Int) =
    val msg = "The transform value of %d is %d"
    msg.format(n, transform(n))
  def findFirst[A](arr: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(i: Int): Int =
      if i >= arr.length then -1
      else if p(arr(i)) then i
      else loop(i + 1)
    loop(0)
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    if as.length <= 1 then true
    else
      @annotation.tailrec
      def inner(as: Array[A], i: Int, j: Int, gt: (A, A) => Boolean): Boolean =
        if j >= as.length then true
        else if gt(as(i), as(j)) then false
        else inner(as, i + 1, j + 1, gt)
      inner(as, 0, 1, gt)
  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
  def compose[A, B, C](f: A => B, g: B => C): A => C =
    a => g(f(a))
