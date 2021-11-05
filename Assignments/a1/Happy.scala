object Happy {
  def sumOfDigitsSquared(n: Int): Int = {
    if (n == 0) 0 else Math.pow(n % 10, 2).toInt + sumOfDigitsSquared(n / 10)
  }

  def isHappy(n: Int): Boolean = {
    if (n == 1) {
      true
    }
    else if (n == 4) {
      false
    }
    else {
      isHappy(sumOfDigitsSquared(n))
    }
  }

  def kThHappy(k: Int): Int = {
    def getNum(n: Int, happy: Boolean, count: Int): Int = {
      if (count == k) {
        n - 1
      }
      else {
        if (happy == false) {
          getNum(n + 1, isHappy(n + 1), count)
        }
        else {
          getNum(n + 1, isHappy(n + 1), count + 1)
        }
      }
    }

    getNum(1, isHappy(1), 0)
  }

  def main(args: Array[String]): Unit = {
    println(sumOfDigitsSquared(199))
    println(isHappy(989))
    println(kThHappy(19))
  }
}
