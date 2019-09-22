package finiteMonoids

import datastructures.Matrix
import helpers.ListHelpers.{combinations, obtainCycle}

case class IntTable(table: Matrix[Int]) {
  // do all rows and columns contain the same unique set of elements?
  def isCayleyTable: Boolean = {
    true
//    val rows = table.getAllRows
//    val cols = table.getAllColumns
//    val someRow = rows.head
//    // is this row a row of unique elements?
//    if (someRow != someRow.distinct) false
//    else
//      rows.map(_.sorted).forall(_ == someRow.sorted) &&
//      cols.map(_.sorted).forall(_ == someRow.sorted)
  }

  def update(i: Int, j: Int)(k: Int): IntTable = IntTable {
    table.update(i,j)(k)
  }

  def toCayleyTable: CayleyTable[Int] = CayleyTable { table.elems }
}

// my thoughts for this are that we will generate all possible cayley tables
// remove duplicates
// and then count the results
// we could also enumerate groups by filtering things to groups if I define an isGroup method
object MonoidEnumeration extends App {
  def getAllPossibleRows(n: Int): List[List[Int]] = {
    val res = (0 until n).toList.permutations.toList
    println(res)
    res
  }

  def allRows(n: Int): List[List[Int]] =
    combinations(n)((0 until n).toList)

  def getAllCayleyTables(n: Int): List[CayleyTable[Int]] = {
    combinations(n)(allRows(n))
      .map(Matrix(_))
      .map(x => { println(x); IntTable(x) })
      .filter(_.isCayleyTable)
      .map(_.toCayleyTable)
  }

  getAllCayleyTables(2).foreach(_.show())
}
