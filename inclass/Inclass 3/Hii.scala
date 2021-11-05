class Hii {

  def sumPairList(xs: List[(Int,Int)]): Int ={
    def sum(p: (Int,Int)) = (p._1 + p._2)
    sum(xs.head)
  }

  def firsts(xs: List[(Int, Int)]): List[Int] = {
    xs.map(_._1)
  }

  def seconds(xs: List[(Int, Int)]): List[Int] = {
    xs.map(_._2)
  }

  def pairSumList(xs: List[(Int, Int)]): (Int, Int) = {
    def sumList(xy: List[Int]): Int = if (xy.isEmpty) 0 else xy.head +
      sumList(xy.tail)

    def sum(x:List[Int]):Int = {
      x match {
        case Nil => 0
        case a :: i => a
      }
    }
    xs match {
      case Nil => (0,0)
      case (a, b) :: t=> (sumList(firsts(xs)),sumList(seconds(xs)))
    }
  }
}

object test
{
  def main(args: Array[String]): Unit = {
    val obj = new Hii
    println(obj.sumPairList(List((5,8),(7,8))))
    println(obj.firsts(List((6,9),(7,8),(99,100))))
    println(obj.seconds(List((6,9),(7,8),(99,100))))
    println(obj.pairSumList(List((6,9),(7,8),(3,5),(4,8))))
  }
}


