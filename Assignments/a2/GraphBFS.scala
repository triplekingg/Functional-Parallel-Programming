object GraphBFS {

  def bfs[V](nbrs: V => Set[V], src: V): (Map[V, V], Map[V, Int]) = {
    //Helper functions defined below
    def iter1(
               helper_frontier: Set[V],
               helper_parent: Map[V, V],
               helper_frontiers: Set[V],
               helper_parents: Map[V, V],
               helper_visited: Set[V]
             ): (Set[V], Map[V, V]) = {
      if (helper_frontier.isEmpty) (helper_frontiers, helper_parents ++ helper_parent)
      else {
        val neighbor = nbrs(helper_frontier.head)
        iter1(
          helper_frontier.tail,
          helper_parent,
          (helper_frontiers ++ neighbor) -- helper_visited,
          iter2(neighbor, helper_frontier.head, helper_parents),
          helper_visited + helper_frontier.head
        )
      }
    }

    def iter2(helper_frontier: Set[V], helper_src: V, helper_parents: Map[V, V]): Map[V, V] = {
      if (helper_frontier.isEmpty) helper_parents
      else iter2(helper_frontier.tail, helper_src, helper_parents + (helper_frontier.head -> helper_src))
    }

    def map_helper(
                    helper_frontier: Set[V],
                    d: Int,
                    distance: Map[V, Int]
                  ): Map[V, Int] = {
      if (helper_frontier.isEmpty) distance
      else {
        map_helper(helper_frontier.tail, d, distance + (helper_frontier.head -> d))
      }
    }

    //Helper functions defined above
    //---------------------------------------------------------------------------------------------
    def expand(
                frontier: Set[V],
                parent: Map[V, V],
                visited: Set[V]
              ): (Set[V], Map[V, V]) = {
      iter1(frontier, parent, Set(), Map(), visited)
    }

    def iterate(
                 frontier: Set[V],
                 parent: Map[V, V],
                 distance: Map[V, Int],
                 d: Int,
                 visited: Set[V]
               ): (Map[V, V], Map[V, Int]) =
      if (frontier.isEmpty)
        (parent, distance + (src -> 0))
      else {

        val couple = expand(frontier, parent, visited)
        val dist = distance ++ map_helper(couple._1, d + 1, Map())
        iterate(couple._1, couple._2, dist, d + 1, visited ++ frontier)
      }

    iterate(Set(src), Map(src -> src), Map(), 0, Set())
  }

}
