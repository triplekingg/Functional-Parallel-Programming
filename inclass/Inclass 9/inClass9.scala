object inClass9 extends App {

  trait RandomnessService {
    def nextRandomNum(): Int
  }

  trait ConstantRandom extends RandomnessService {
    def nextRandomNum() = 26
  }

  trait IncrementRandom extends RandomnessService {
    def nextRandomNum() = {
      26+1
    }
  }
  trait SeedProvider {
    def RandomSeed: Long
  }

  trait PseudoRandom extends RandomnessService {
    this: SeedProvider =>
    val RNG = new scala.util.Random(RandomSeed)
    def nextRandomNum() = RNG.nextInt()
  }

  class LuckyDraw {
    this: RandomnessService =>
    def showLuckyNumbers(n: Int) = {
      def iterate(n: Int, cc: List[Int] => List[Int]): List[Int] =
        n match {
          case 0 => cc(Nil)
          case _ => iterate(n - 1, (lst: List[Int]) => cc(n :: lst))
        }
      val luckyNumbers =
        iterate(n, (x: List[Int]) => x).map(k => this.nextRandomNum())
    }
  }
  val constantDraw = new LuckyDraw with ConstantRandom
  val incrDraw = new LuckyDraw with IncrementRandom
  val pseudoDraw = new LuckyDraw with PseudoRandom with SeedProvider {
    val RandomSeed = 1409
  }

  val numLucky = 5

  println("constantDraw:")
  constantDraw.showLuckyNumbers(numLucky)
  println("incrDraw:")
  incrDraw.showLuckyNumbers(numLucky)
  println("pseudoDraw:")
  pseudoDraw.showLuckyNumbers(numLucky)


}

