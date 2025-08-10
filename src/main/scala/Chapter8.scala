opaque type Gen[+A] = State[RNG, A]

import Prop.*
trait Prop:
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

object Prop:
  opaque type FailedCase = String
  opaque type SuccessCount = Int

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    RNG.nonNegativeInt.map(num => {
      val m = stopExclusive - start
      val adjusted = num % m 
      adjusted + start
    })

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    int.map(b => if b >= 0 then true else false)

  extension [A](self: Gen[A]) 
    def listOfN(n: Int): Gen[List[A]] =
     State.sequence(List.fill(n)(self)) 
      
