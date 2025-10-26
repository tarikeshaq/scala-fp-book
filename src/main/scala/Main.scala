import java.util.concurrent.Executors

@main def hello(): Unit =
  val l = None 
  val res = Monad.optionMonad.replicateM(l)(5) 
  println(res)
