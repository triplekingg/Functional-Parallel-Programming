object Spaghetti extends App {

  //The logic is correct but i dont know how to implement it in lazy list form :(
  def spaghetti: List[String] = {
    //Generator function
    def generate(lst: List[String], index: Int, newlst:List[String]): List[String] = {
      //Parser function
      def parser(str: String): String = {
        val len = str.length
        //Counter function
        def helper(str:String, start:Int, end:Int, count:Int, result:String):String = {
          val len =str.length
          if(len==1) result.appended('1').appended(str(start))
          if(end == len){
            val res:String = count.toString + str(start)
            result.appended(res(0)).appended(res(1))
          }
          else{
            if(str(start)==str(end)){
              helper(str,start,end+1,count+1,result)
            }
            else{
              val res:String = count.toString + str(start)
              helper(str,end,end+1,1,result.appended(res(0)).appended(res(1)))
            }
          }
        }
        helper(str,0,1,1,new String)
      }
      generate(lst,index+1, parser(lst(index))::newlst)
    }
    generate(List("1"),1,List())
  }

  def change(k: String, buffer: String): String =
    k.toList match {
      case h :: next if (h == '0') =>
        change(next.mkString(""), buffer + '1')
      case h :: next if (h == '1') =>
        change(next.mkString(""), buffer + '0')
      case Nil => buffer
    }

  def H(n: Int): List[String] = {
    if (n == 0) {
      List("")
    } else {
      List.concat(
        H(n - 1).map(x => '0' + x),
        H(n - 1).map(x => '0' + x).map(b => change(b, ""))
      )
    }
  }

  def ham: LazyList[String] = {
    def loop(number: Int): LazyList[String] = {
      LazyList.from(H(number)) #::: loop(number + 1)
    }
    loop(1)
  }

}
