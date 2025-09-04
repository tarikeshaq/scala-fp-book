import State.*

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  for
    _ <- sequence(inputs.map(_ match {
      case Input.Coin =>
        modify((s: Machine) =>
          s match {
            case Machine(locked, candies, coins) if locked && candies > 0 =>
              Machine(false, candies, coins + 1)
            case Machine(locked, candies, coins) =>
              Machine(locked, candies, coins + 1)
          }
        )
      case Input.Turn =>
        modify((s: Machine) =>
          s match {
            case Machine(locked, candies, coins) if !locked && candies > 0 =>
              Machine(true, candies - 1, coins)
            case _ => s
          }
        )
    }))
    s <- get
  yield (s.coins, s.candies)
