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

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if(i >= as.length - 1) true
      else if(gt(as(i), as(i+1))) false
      else loop(i + 1)
    }
    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)

  }

  def uncurry[A,B,C](f: A => B => C): (A,B) => C ={
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
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
