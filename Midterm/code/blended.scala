object blended extends App {
  sealed trait Dessert

  case class Pie(kind: String) extends Dessert

  case class Smoothie(fruits: List[String]) extends Dessert

  case class Cake(toppings: String) extends Dessert

  def blend(what: Dessert): String = {
    what match {
      case smoothie: Smoothie => "Blended Smoothie"
      case pie: Pie => "Blended " + blended.pie.kind.toString + " Pie"
      case cake: Cake => "Blended " + blended.cake.toppings.toString + " Cake"
      case _ => "Not a dessert"
    }
  }

  val smoothie = Smoothie(List("apple", "milk", "banana"))
  val pie = Pie("Carrot")
  val cake = Cake("Vanilla")

  println(blend(smoothie))
  println(blend(pie))
  println(blend(cake))

}
