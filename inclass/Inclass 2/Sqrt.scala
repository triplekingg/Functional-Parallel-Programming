import scala.math._
import scala.language.postfixOps

object Sqrt extends App {

  def roundAt(p: Int)(n: Double): Double = {
    val s = math.pow(10, p);
    (math.round(n * s)) / s
  }

  def roundAt5(n: Double) = roundAt(5)(n)

  def isGoodEnough(guess: Double, y: Double) = math.abs(guess * guess - y) / y < 1e-10

  def improved(guess: Double, y: Double) = (guess + y / guess) / 2

  def newtonsMethod(guess: Double, y: Double): Double =
    if (isGoodEnough(guess, y)) guess
    else newtonsMethod(improved(guess, y), y)

  def sqrt(y: Double): Double = newtonsMethod(1, y)

  println("sqrt(144) = " + roundAt5(sqrt(144)))
}
