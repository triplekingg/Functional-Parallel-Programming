

object sortedlist extends App {

  class SortedList(lst: List[Int]) {

    def insertionSort(list: List[Int]): List[Int] = {
      def insertSorted(element: Int, sortedList: List[Int]): List[Int] =
        if (sortedList.isEmpty || element <= sortedList.head) element :: sortedList
        else sortedList.head :: insertSorted(element, sortedList.tail)

      if (list.isEmpty || list.tail.isEmpty) list
      else insertSorted(list.head, insertionSort(list.tail))
    }

    def l = insertionSort(lst)

    def length = l.length

    def display = l

  }

  def insertListCPS(x: Int, L: SortedList): SortedList = {
    new SortedList(x :: L.l)
  }

  def search(x: Int, L: SortedList): Boolean = {
    if (L.l.isEmpty) false
    else {
      if (L.l.head == x) true
      else search(x, new SortedList(L.l.tail))
    }
  }

  def filterCPS(x: Int, L: SortedList): SortedList = {
    new SortedList(L.l.filter(n => n < x))
  }

  def cleanUpListCPS(L: SortedList): SortedList = {
    def isSorted[T <% Ordered[T]](a: List[T]): Boolean =
      if (a == Nil) true // an empty list is sorted
      else a.foldLeft((true, a.head))(
        (prev, v1) => {
          val (p, v0) = prev
          (p && v0 <= v1, v1)
        })._1

    val sort = isSorted(L.l)
    if (sort) new SortedList(L.l)
    else new SortedList(L.l.sorted)
  }

  def test_cases() = {
    val a = new SortedList(List(1, 4, 92, 3))
    val b = new SortedList(List(3, 5, 1112, 3, 46, 543))
    val c = new SortedList(List())
    println(insertListCPS(5, a).display) //Insert element but still maintaining the property
    println(insertListCPS(1, c).display) //Insert element but still maintaining the property
    println(search(4, a)) //Seaching for an element that is present
    println(search(4, b)) //Seaching for an element that is not present
    println(search(3, c)) //Searching empty list
    println(cleanUpListCPS(b).display) //Clean b
    println(filterCPS(50, b).display) //Filtering only elements less than number 50

  }

  test_cases()

}
