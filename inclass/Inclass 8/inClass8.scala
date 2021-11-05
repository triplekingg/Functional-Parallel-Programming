trait Tree
case class Node(l: Tree, k: Int, r: Tree) extends Tree
case object Empty extends Tree

object InClass8 extends App {
  def fibonacciCPS(n: Int) = {
    def h1(n: Int, xx: Int => Int): Int =
      n match {
        case 0 => xx(0)
        case 1 => xx(1)
        case _ => h1(n - 1, (x: Int) => h1(n - 2, (y: Int) => xx(x + y)))
      }
    h1(n, ((x: Int) => x))
  }

  def preorderWalk (tree: Tree): List[Int] = {
    def inspect(t: Tree, lst: List[Int] => List[Int]): List[Int] =
      t match {
        case Empty => lst(Nil)
        case Node(left, key, right) =>
          inspect(
            left,
            leftList => {
              inspect(right, rightList =>
                lst(key :: (leftList ::: rightList)))
            }
          )
      }
    inspect(tree, (r: List[Int]) => r)
  }

  val tree = Node(
    Node(Empty, 3, Empty),
    7,
    Node(Node(Empty, 9, Empty), 15, Node(Empty, 2, Empty))
  )

  println(preorderWalk (tree))
}