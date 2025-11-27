import Console.readLn
import Console.printLn
@main def hello(): Unit =
  val foo = for
    ln <- readLn
    _ <- printLn(ln.getOrElse("wow"))
  yield ()
  foo.toThunk()
