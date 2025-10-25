import java.util.concurrent.Executors

@main def hello(): Unit =
  val es = Executors.newFixedThreadPool(5)
  val words = "Hello there man, fhi! h"
  val res = wordCount(words) 
  println(res.run(es))

// Goal:
// Determine if an IndexedSeq is sorted by using foldMap
//
// Core concept: Our Monoid will need to communicate both the maximum
// value + whether it's sorted so far
// eg: if we have 1, 2, 3, 4
// using a left foldMap, our Monoid is Monoid[(Int, Boolean)]
// That compares two values, and returns the larger, and whether the one on the right is larger
// The empty value is INT.min, true
val sortMonoid: Monoid[(Int, Boolean)] = new:
  def combine(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) = (a1, a2) match {
    case ((a1Num, isFirstSorted), (a2Num, isSecondSorted)) if a2Num >= a1Num && isFirstSorted && isSecondSorted => (a2Num, true)
    case ((a1Num, _), (a2Num, _)) => (Math.max(a1Num, a2Num), false)
  }
  def empty: (Int, Boolean) = (Int.MinValue, true)

