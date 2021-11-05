import scala.annotation.tailrec

object Zombies {
    def countBad(hs: List[Int]): Int = {
        def helper(hs1: List[Int], counter: Int): (List[Int], Int) = {
            def merger(left: List[Int], right: List[Int], count: Int): (List[Int], Int) =
                (left, right) match {
                    case (l, Nil) => (l, count)
                    case (Nil, r) => (r, count)
                    case (h :: t, x :: y) =>
                        if (x >= h) {
                            val z = merger(left, y, count + left.size)
                            (x :: z._1, z._2)
                        }
                        else {
                            val z = merger(t, right, count)
                            (h :: z._1, z._2)
                        }
                }
            if (hs1.length / 2 == 0) {
                (hs1, counter)
            }
            else {
                val l = hs1.slice(0,hs1.length/2)
                val l_MergeSort = helper(l, counter)
                val r = hs1.slice(hs1.length/2,hs1.length)
                val r_MergeSort = helper(r, counter)
                merger(l_MergeSort._1, r_MergeSort._1, l_MergeSort._2 + r_MergeSort._2)
            }
        }
        helper(hs, 0)._2
    }

    def main(args: Array[String]): Unit = {
        // Test your code here
        val t1 = List(35,22,10)
        val t2 = List(3,1,4,2)
        val t3 = List(5,4,11,7)
        val t4 = List(1,7,22,13,25,4,10,34,16,28,19,31)
        println(countBad(t1))
        println(countBad(t2))
        println(countBad(t3))
        println(countBad(t4))
    }
}
