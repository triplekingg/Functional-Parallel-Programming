object FilterMap extends App {
  def map[A, B](f: (A) => B, xs: List[A]): List[B] = if (xs.isEmpty) Nil else f(xs.head) :: map(f, xs.tail)

  def filter[A](p: (A) => Boolean, xs: List[A]): List[A] = {
    if (xs.isEmpty) Nil else {
      if (p(xs.head)) xs.head :: filter(p, xs.tail) else filter(p, xs.tail)
    }
  }

  val xs = List(3, 2, 5, 7, 1, 9) // xs: List[Int]
  val f = (x: Int) => "z" + x.toString // f: Int => String
  val ys = xs.map(f)
  println(ys)

  val xs1 = List(3, 2, 5, 7, 1, 9) // xs: List[Int]
  val f1 = (x: Int) => x % 2 == 1 // returns True if x is odd
  val ys1 = xs1.filter(f1)
  println(ys1)
}
