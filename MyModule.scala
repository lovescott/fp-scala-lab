object MyModule {
  def abs(n: Int): Int = {
    if (n < 0)
      -n
    else
      n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, num: Int, fibnum: Int): Int = {
      if (n <= 0) fibnum
      else go(n - 1, fibnum, fibnum + num)
    }
    go(n, 1, 0)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value",-42, (x: Int) => abs(x)))
    println(formatResult("factorial", 7, (x: Int) => factorial(x)))
  }



}
