package finiteMonoids

// this is a simple way of representing the structure of a finite monoid
// https://en.wikipedia.org/wiki/Cayley_table
case class CayleyTable[A](table: List[List[A]]) {
  import CayleyTable._

  def show: Unit = {
    println("Cayley Table:")
    table.foreach(row => {
      println(row.map(_.toString).reduce((x,y) => s"${x}, ${y}"))
    })
  }

  def toSortedNumericTable: CayleyTable[Int] = {
    val rep = table.flatten.distinct.zipWithIndex.toMap[A, Int]
    CayleyTable {
      val intTable = deepMap(rep)(table)
      intTable.sortBy(_.head)
    }
  }
}

object CayleyTable {
  def deepMap[A,B](f: A => B)(xss: List[List[A]]): List[List[B]] =
    xss.map(_.map(f))

  // need to compare one Cayley table with all permutations of the other
  // all we're trying to establish is if they have the same structure, not the same contents
  def areIsomorphic[A,B](as: CayleyTable[A],  bs: CayleyTable[B]): Boolean = {
    val List(aNums, bNums) = List(as, bs).map(_.toSortedNumericTable)
    getAllPermutations(aNums).contains(bNums)
  }

  def permuteCayleyTable(numericTable: CayleyTable[Int]): CayleyTable[Int] = {
    val table = numericTable.table
    val mod = table.length

    def nextInt(x: Int) = if (x == mod - 1) 0 else x + 1

    CayleyTable { deepMap(nextInt)(table) }
  }

  // this is obscenely dangerous because if you call it with something that never returns to its initial state you die
  @annotation.tailrec
  def obtainCycle[A](init: A, f: A => A, res: List[A]): List[A] = res match {
    case Nil => obtainCycle(init, f, List(init))
    case h :: Nil => obtainCycle(init, f, f(h) :: (h :: Nil))
    case h :: _ => {
      val next = f(h)
      if (next == init) res else obtainCycle(init, f, next :: res)
    }
  }

  def getAllPermutations(numericTable: CayleyTable[Int]): List[CayleyTable[Int]] =
    obtainCycle(numericTable, permuteCayleyTable, Nil)
}
