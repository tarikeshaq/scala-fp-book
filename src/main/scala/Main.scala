import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.ExecutorService
import java.util.concurrent.ForkJoinPool


@main def hello(): Unit =
  val ls = List("Hello there man", "how are you doing today") 
  val parRes = numWords(ls) 
  val res = parRes.run(ForkJoinPool.commonPool())
  println(res.get)

