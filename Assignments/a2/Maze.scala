object Maze {
  def solveMaze(maze: List[String]): Option[String] = {
    def find_direction(
                   lst_node: List[Int],
                   map: Map[Int, Set[Int]],
                   lst_nodes: List[Int]
                 ): Map[Int, Set[Int]] =
      lst_node match {
        case head :: next => {
          find_direction(
            next,
            map + (head -> Set(
              head - 1,
              head + 1,
              head - maze.head.length,
              head + maze.head.length
            ).filter(r => lst_nodes.contains(r))),
            lst_nodes
          )
        }
        case Nil => map
      }
    val g = GraphBFS
    Some("result")
  }
}
