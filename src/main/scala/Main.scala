@main def hello(): Unit =
  val rng: RNG = SimpleRNG(42)
  val (n1, nextRNG) = rng.nextInt
  println(n1)
  val machineState = simulateMachine(List(Input.Coin, Input.Turn, Input.Coin, Input.Turn))
  val res = machineState.run(Machine(locked = true, candies = 5, coins = 10))
  println(res)

