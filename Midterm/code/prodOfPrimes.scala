import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.Success

object prodOfPrimes extends App {

  def isPrime(n: Int): Boolean = {
    def iterate(num: Int, div: Int): Boolean = {
      if (num < 2) false
      else if (num % 2 == 0) return (num == 2)
      else if ((div * div) > num) true
      else if (num % div == 0) false
      else iterate(num, div + 2)
    }

    iterate(n, 3)
  }

  def findPrimes(x: Int, y: Int): List[Int] = {
    def iterate(x2: Int, y2: Int, xs: List[Int]): List[Int] = {
      if (x2 == y2 + 1) xs.reverse
      else {
        if (isPrime(x2)) iterate(x2 + 1, y2, x2 :: xs)
        else iterate(x2 + 1, y2, xs)
      }
    }

    iterate(x, y, List())
  }

  def profOfPrime(): Int = {
    val primes = findPrimes(10000000, 20000000)
    val random = new scala.util.Random
    val start = 0
    val end = primes.length
    val r1 = start + random.nextInt((end - start) + 1)
    val r2 = start + random.nextInt((end - start) + 1)
    primes(r1) * primes(r2)
  }

  //  def betterProfOfPrime():  Future[List[Int]]= {
  //    def helper(list: List[Any]): List[Any] = Future{
  //      def helper2(n1:Int, n2:Int, lst: List[Any]): List[Int] = {
  //        if(n2==20000000) list
  //        else helper2(n2+1,n2+1000, findPrimes(n1,n2)::list)
  //      }
  //    }

  //One way is to do this
  // val a = Future{findPrimes(10000000,10001000)}
  // val b = Future{findPrimes(10001001,10002000)}
  // val c = Future{findPrimes(10002001,10003000)}
  // val d = Future{findPrimes(10003001,10004000)}
  // val e = Future{findPrimes(10004001,10005000)}
  //These can be done recursively to calculate in parallel way,
  //and return a list of the prime numbers between 10000000 and 20000000
  //In this way the list will be calculated very fast
  //However I am not being able to implement this logic.

  //  }

  //TestCases, everything works properly except for the last function
  println(isPrime(0))
  println(isPrime(2))
  println(isPrime(3))
  println(isPrime(5))
  println(isPrime(6))
  println(isPrime(7))
  println(isPrime(99))
  println(isPrime(54))
  println(isPrime(53))
  println(isPrime(47))
  println(isPrime(37))
  println(findPrimes(2, 7))
  println(findPrimes(100, 900))
  println(profOfPrime)
  //  println(betterProfOfPrime())


}
