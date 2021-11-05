class Exercise4 {
  def find(xs: List[(Int, String)], key: Int): Option[String] = {
    if (xs.isEmpty) None else {
      if (xs.head._1 == key) Some(xs.head._2) else find(xs.tail, key)
    }
  }

  def rev(xs: List[Int]): List[Int] = {
    def tailrecRev(xs: List[Int], acc: List[Int]): List[Int] =
      if (xs.isEmpty) acc
      else {
        tailrecRev(xs.tail, xs.head :: acc)
      }
    tailrecRev(xs, List())
  }

  def fib(n: Int): Long = {
    def tailrecFib(n: Int, acc_1: Int, acc_2: Int): Long = {
      if (n == 0) acc_1
      else if (n == 1) acc_2
      else tailrecFib(n - 1, acc_2, acc_1 + acc_2)
    }

    tailrecFib(n, 0, 1)
  }
}
object blah{
  def main(args: Array[String]): Unit = {
    val obj = new Exercise4
    val l1 = List((1, "w"), (2, "x"), (3, "y"), (4, "z"))
    val l2 = List(1,2,3,4,5,6,7,8,9)
    println(obj.find(l1, 3))
    println(obj.rev(l2))
    println(obj.fib(6))
  }
}



