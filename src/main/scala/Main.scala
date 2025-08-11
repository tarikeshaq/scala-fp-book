@main def hello(): Unit =
  val p = Prop.forAll(Gen.boolean)(x => x == x) 
  val q = Prop.forAll(Gen.boolean)(x => x)

  (q || q).run

