
object TurnIt {
    def transpose(A: List[List[Int]]): List[List[Int]] = {
        val matrix = A.toArray
        val rows = A.length
        val columns = A.head.length
        def transpose_helper(cr:Int, cc:Int, A: Array[List[Int]],row_acc: List[Int], final_acc: List[List[Int]]): List[List[Int]] = {
            if(cr==rows && cc==columns){
                row_acc::final_acc
            }
            else if(cr==rows-1 && cc==columns-1){
                transpose_helper(cr+1, cc+1 , A, A(cr)(cc)::row_acc,row_acc::final_acc)
            }
            else{
                if(cr!=rows && cc!=columns){
                    transpose_helper(cr+1, cc, A, A(cr)(cc)::row_acc, final_acc)
                }
                else {
                    transpose_helper(0,cc+1,A,List(), row_acc::final_acc)
                }
            }
        }
          //Since the transpose_helper gives the transpose but in reverse, I decided to create 2 functions to help reverse the changes.
        def rev_rows(xs: List[Int]): List[Int] = {
            def tailRev(xs: List[Int], acc: List[Int]): List[Int] =
                if (xs.isEmpty) acc
                else {
                    tailRev(xs.tail, xs.head :: acc)
                }
            tailRev(xs, List())
        }
        def rev_list(xs:List[List[Int]]): List[List[Int]] ={
            def rev(xs:List[List[Int]], acc:List[List[Int]]): List[List[Int]] = {
                if(xs.isEmpty) acc
                else{
                    rev(xs.tail, rev_rows(xs.head)::acc)
                }
            }
            rev(xs, List())

        }
        val l = transpose_helper(0,0,matrix,List(),List())
        rev_list(l.patch(1,Nil,1))

    }


    def main(args: Array[String]): Unit = {
		// Test your code here
        val t = List(List(1,2,3),List(4,5,6),List(7,8,9))
        println(transpose(t))
    }
}

