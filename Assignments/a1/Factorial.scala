object Factorial {
  def factorial(n: Int): Long = {
    if (n == 0) 1 else n * factorial(n - 1)
  }

  def main(args: Array[String]): Unit = {
    print(factorial(19))
  }
}
