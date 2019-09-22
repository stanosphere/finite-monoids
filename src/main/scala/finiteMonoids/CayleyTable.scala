package finiteMonoids

import helpers.ListHelpers.{deepMap, zipWith, obtainCycle}
import datastructures.Matrix

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

  def toNumericTable: CayleyTable[Int] = CayleyTable {
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

  private def getAllPermutations(numericTable: CayleyTable[Int]): List[CayleyTable[Int]] =
    obtainCycle(numericTable, permuteCayleyTable, Nil)

  def toFiniteMonoid(numericTable: CayleyTable[Int]): FiniteMonoid[Int] = {
    val elems = numericTable.table.head
    // just need to look up entry in cayley table
    // for convenience I think I'll just turn it into a matrix
    val matrix = Matrix(numericTable.table)

    new FiniteMonoid[Int] {
      def elements: List[Int] = elems
      def op(x: Int, y: Int): Int = matrix.getElem(x, y)
      def zero: Int = 0
    }
  }
}
