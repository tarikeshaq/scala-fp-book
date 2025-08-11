@main def hello(): Unit =
  val smallInt = Gen.choose(-10, 10)
  
  val maxProp = Prop.forAll(smallInt.nonEmptyList): l =>
    val max = l.max
    l.forall(_ <= max)

  val sortProp = Prop.forAll(smallInt.list): l =>
    val sorted = l.sorted
    sorted.foldLeft((Int.MinValue, true)) {
      case ((acc, sorted), curr) => if curr < acc then (curr, false) else (curr, true)
    }._2

  sortProp.run 

