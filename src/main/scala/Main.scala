@main def hello(): Unit =
  val jParser = parseJson(ParsersImpl)

  import ParsersImpl.*

  println(jParser.run("{\"hello\": [1, 2, 3], \"damn\": null}"))
