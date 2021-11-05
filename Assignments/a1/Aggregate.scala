object Aggregate {
  def myMin(p: Double, q: Double, r: Double): Double = {
    Math.min(p, Math.min(q, r))
  }

  def myMean(p: Double, q: Double, r: Double): Double = {
    (p + q + r) / 3
  }

  def myMed(p: Double, q: Double, r: Double): Double = {
    def sort(a: Double, b: Double, c: Double): List[Double] = {
      List(a, b, c).sorted
    }

    def num(l: List[Double]): Double = {
      l(1)
    }

    num(sort(p, q, r))
  }

  def main(args: Array[String]): Unit = {
    println(myMin(3.0, 1.0, 9.0))
    println(myMean(3, 7, 4))
    println(myMed(4, 1, 5))
  }
}
