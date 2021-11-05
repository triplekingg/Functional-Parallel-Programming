
class inclass6 {

  def unzip[T](zipped: List[(Option[T], Option[T])]): (List[T], List[T]) = {
    zipped match {
      case Nil => (Nil, Nil)
      case a :: x => (a._1.head :: unzip(x)._1, a._2.head :: unzip(x)._2)
    }
  }


  def countWhile[T](xs: List[T], key: T): Int = {
    def keepCount[T](xs: List[T], key: T, count: Int): Int = {
      if (xs.isEmpty) {
        count
      }
      else {
        if (xs.head == key) {
          keepCount(xs.tail, key, count + 1)
        }
        else {
          keepCount(xs.tail, key, count)
        }
      }
    }

    keepCount(xs, key, 0)
  }

  def topK(xs: List[Int], k: Int): List[Int] = {
    def helper(xs: List[Int], ys: List[(Int, Int)], count: Int, index: Int): List[(Int, Int)] = {
      if (index == xs.length - 1) {
        (count, xs(index)) :: ys
      }
      else {
        if (xs(index) == xs(index + 1)) {
          helper(xs, ys, count + 1, index + 1)
        }
        else {
          helper(xs, (count, xs(index)) :: ys, 1, index + 1)
        }
      }
    }

    val l = helper(xs.sorted, List(), 1, 0).sorted.sortBy(_._1).reverse

    def extract(xs: List[(Int, Int)], ys: List[Int], k: Int, index: Int): List[Int] = {
      if (k == 0) {
        ys
      }
      else {
        extract(xs, xs(index)._2 :: ys, k - 1, index + 1)
      }
    }

    extract(l, List(), k, 0).reverse
  }


  trait Dessert

  case class Pie(kind: String) extends Dessert

  case class Smoothie(fruits: List[String]) extends Dessert

  case class Cake(toppings: String) extends Dessert


  def isLiquid(what: Dessert): Boolean = {
    what match {
      case smoothie: Smoothie => true
      case _ => false
    }
  }
}


object test {
  def main(args: Array[String]): Unit = {
    val obj = new inclass6
    val smoothie = obj.Smoothie(List("apple", "milk", "banana"))
    val testing1 = List("hi", 1, 2, 3, 4, "hi", "hello", "hi")
    val topk = List(1, 1, 1, 1, 9, 9, 9, 9, 9, 9, 9, 3, 3, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7)
    val testing = List((Option("hi"), Option(1.2)), (Option(2), Option(1)), (Option(5), Option(9)))
    println(obj.unzip(testing))
    println(obj.countWhile(testing1, "hi"))
    println(obj.isLiquid(smoothie))
    println(obj.topK(topk, 3))
  }
}
