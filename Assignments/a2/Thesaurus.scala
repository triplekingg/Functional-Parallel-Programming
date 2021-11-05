import scala.io.Source.fromFile

object Thesaurus {

  val defaultEncoding = "ISO8859-1"

  def iter(
            lst: List[String],
            num: Int,
            start: String,
            syn: Set[String],
            map: Map[String, Set[String]]
          ): Map[String, Set[String]] =
    lst match {
      case hd :: next if (num < 1) => {
        val argument = hd.split('|').toList
        val a = argument.tail.head.trim.toInt
        val b = argument.head.trim
        iter(
          next,
          a,
          b,
          Set(),
          map + (start -> syn)
        )
      }
      case strt :: next => {
        val ls = strt.split('|').toList.tail
        val converted = ls.tail.toSet
        val st = converted.map((elm: String) => elm.trim)
        iter(next, num - 1, start, syn ++ st, map)
      }
        if(lst.isEmpty) map
    }

  def load(filename: String): String => Set[String] = {
    val text = fromFile(filename)(defaultEncoding)
    val ln = text.getLines().toList.tail
    val file_map: Map[String, Set[String]] = iter(ln, 0, "", Set(), Map())
    (validity: String) => file_map.get(validity).fold(Set[String]())(_ ++ Set[String]())
  }

  def linkage(
               thesaurusFile: String
             ): String => String => Option[List[String]] = {
    val thesaurus = load(thesaurusFile)
    (first_word: String) => {
      val least = GraphBFS.bfs[String](thesaurus, first_word)
      val each = least._1
      (second_word: String) => {
        def move(
                  start: String,
                  end: String,
                  results: List[String]
                    ): Option[List[String]] = {
          if (start.contentEquals(end)) Some(results)
          else {
            val e1 = each.get(start).fold("")(_ + "")
            if (!e1.isEmpty) move(e1, end, results :+ e1) else None
          }
        }
        move(second_word, first_word, List(second_word))
      }
    }
  }
}
