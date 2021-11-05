package solver
import scala.math._
import scala.language.postfixOps

object Newton {

    def goodEnough(guess: Double): Boolean = (math.abs(math.log(guess) - 1)) < 1e-10

    def improve(guess: Double): Double = guess - (math.log(guess) - 1) * guess

    def repeat(guess: Double): Double =
      if (goodEnough(guess)) guess
      else repeat(improve(guess))

    def computeE = repeat(1.0)

    println("computeE = " + computeE)



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





  // your implementation of the Newton method that takes a function f: Double => Double
  // and its derivative df: Double => Double  (take note of the types),
  // and computes a root of f using the Newton's method with the given 
  // guess: Double starting value

  def solve(f: Double => Double, df: Double => Double,
            guess: Double = 1.0): Option[Double] = {
    solve(newtonsMethod())
  }

}
