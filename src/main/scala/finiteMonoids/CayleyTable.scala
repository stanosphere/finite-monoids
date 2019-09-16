package finiteMonoids

import helpers.ListHelpers.{deepMap,zipWith}

// this is a simple way of representing the structure of a finite monoid
// https://en.wikipedia.org/wiki/Cayley_table
case class CayleyTable[A](table: List[List[A]]) {
  import CayleyTable._

  def show(): Unit = {
    println("Cayley Table:")
    table.foreach(row => {
      println(row.map(_.toString).reduce((x,y) => s"${x}, ${y}"))
    })
  }

  def showPermutations(): Unit = {
    println("Permutations:")
    val numericTable = CayleyTable(table).toNumericTable
    getAllPermutations(numericTable)
      .map(sortForComparison)
      .foreach(_.show)
  }

  private def toNumericTable: CayleyTable[Int] = CayleyTable {
    val rep = table.flatten.distinct.zipWithIndex.toMap[A, Int]
    deepMap(rep)(table)
  }
}

object CayleyTable {
  private def comparator(xs: List[Int], ys: List[Int]): Boolean = {
    zipWith(xs,ys)((_,_)).foldLeft(false)(
      (bool, x) => bool || { val (l,r) = x; l < r }
    )
  }

  private def sortColumns(c: CayleyTable[Int]): CayleyTable[Int] =
    CayleyTable { c.table.transpose.sortWith(comparator).transpose }

  private def sortRows(c: CayleyTable[Int]): CayleyTable[Int] =
    CayleyTable { c.table.sortWith(comparator) }

  private def sortForComparison(c: CayleyTable[Int]): CayleyTable[Int] =
    (sortRows _ andThen sortColumns)(c)

  // need to compare one Cayley table with all permutations of the other
  // all we're trying to establish is if they have the same structure, not the same contents
  def areIsomorphic[A,B](as: CayleyTable[A],  bs: CayleyTable[B]): Boolean = {
    val List(aNums, bNums) = List(as, bs).map(_.toNumericTable)
    val sortedB = sortForComparison(bNums)
    getAllPermutations(aNums).map(sortForComparison).contains(sortedB)
  }

  private def permuteCayleyTable(numericTable: CayleyTable[Int]): CayleyTable[Int] = {
    val table = numericTable.table
    val mod = table.length

    def nextInt(x: Int) = if (x == mod - 1) 0 else x + 1

    CayleyTable { deepMap(nextInt)(table) }
  }

  // this is obscenely dangerous because if you call it with something that never returns to its initial state you die
  @annotation.tailrec
  private def obtainCycle[A](init: A, f: A => A, res: List[A]): List[A] = res match {
    case Nil => obtainCycle(init, f, List(init))
    case h :: Nil => obtainCycle(init, f, f(h) :: (h :: Nil))
    case h :: _ => {
      val next = f(h)
      if (next == init) res else obtainCycle(init, f, next :: res)
    }
  }

  private def getAllPermutations(numericTable: CayleyTable[Int]): List[CayleyTable[Int]] =
    obtainCycle(numericTable, permuteCayleyTable, Nil)
}
