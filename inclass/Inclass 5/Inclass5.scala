class Inclass5 {

  def zip(x : List[Int], y: List[Int]) : List[(Option[Int], Option[Int])] = {
    (x,y) match {
      case (Nil,Nil) => Nil
      case (h1::x, h2::y) => (Some(h1),Some(h2)) :: zip(x,y)
    }
  }

  def unzip(zipped : List[(Option[Int], Option[Int])]) : (List[Int], List[Int]) = {
    zipped match {
      case Nil => (Nil,Nil)
      case a :: x => (a._1.head :: unzip(x)._1 , a._2.head :: unzip(x)._2)
    }
  }

}

object test
{
  def main(args: Array[String]): Unit = {
    val obj = new Inclass5
    val testing = List((Option(3), Option(6)), (Option(2), Option(1)), (Option(5), Option(9)))
    println(obj.zip(List(3,2,5), List(6,1,9)))
    println(obj.unzip(testing))
  }
}
