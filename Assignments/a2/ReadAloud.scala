object ReadAloud extends App {

  def readAloud(lst: List[Int]): List[Int] = {
    if (lst.isEmpty) {
      lst
    }
    else {
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

      helper(lst, List(), 1, 0).reverse.flatMap(t => List(t._1, t._2))
    }
  }


  def unreadAloud(lst: List[Int]): List[Int] = {
    def helper(lst: List[Int], counter: Int, index: Int, result: List[Int]): List[Int] = {
      def fill(res: List[Int], c_val: Int, i_val: Int): List[Int] = {
        if (c_val == 0 && index == lst.length - 1) {
          return res.reverse
        }
        if (c_val == 0) {
          helper(lst, counter + 2, index + 2, res)
        }
        else
          fill(i_val :: res, c_val - 1, i_val)
      }

      if (index == lst.length - 1) {
        val counter_val = lst(counter)
        val index_val = lst(index)
        fill(result, counter_val, index_val)
      }
      else {
        val counter_val = lst(counter)
        val index_val = lst(index)
        fill(result, counter_val, index_val)
      }
    }

    helper(lst, 0, 1, List())
  }

  //  val testing = List(5, 3, 2, 4, 6, 7)
  //  println(readAloud(unreadAloud(testing)))
  //  println(unreadAloud(testing))


}
