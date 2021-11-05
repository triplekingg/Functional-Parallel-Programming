import scala.math._

object ComputeE extends App {

  def goodEnough(guess: Double): Boolean = (math.abs(math.log(guess) - 1)) < 1e-10

  def improve(guess: Double): Double = guess - (math.log(guess) - 1) * guess

  def repeat(guess: Double): Double =
    if (goodEnough(guess)) guess
    else repeat(improve(guess))

  def computeE = repeat(1.0)

  println("computeE = " + computeE)

}
