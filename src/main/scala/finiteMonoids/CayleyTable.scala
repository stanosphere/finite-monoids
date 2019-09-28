package finiteMonoids

import scala.math.Ordering.Implicits._

import helpers.ListHelpers.{deepMap, zipWith, obtainCycle, combinations}
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
      .map(sortTable)
      .foreach(_.show)
  }

  def toNumericTable: CayleyTable[Int] = CayleyTable {
    val rep = table.flatten.distinct.zipWithIndex.toMap[A, Int]
    deepMap(rep)(table)
  }
}

object CayleyTable {
  def sortColumns(c: CayleyTable[Int]): CayleyTable[Int] =
    CayleyTable { c.table.transpose.sorted.transpose }

  def sortRows(c: CayleyTable[Int]): CayleyTable[Int] =
    CayleyTable { c.table.sorted }

  def sortTable(c: CayleyTable[Int]): CayleyTable[Int] =
    (sortRows _ andThen sortColumns)(c)

  // need to compare one Cayley table with all permutations of the other
  // all we're trying to establish is if they have the same structure, not the same contents
  def areIsomorphic[A,B](as: CayleyTable[A],  bs: CayleyTable[B]): Boolean = {
    val List(aNums, bNums) = List(as, bs).map(_.toNumericTable)
    val sortedB = sortTable(bNums)
    getAllPermutations(aNums).map(sortTable).contains(sortedB)
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

  def hasIdentityRow(numericTable: CayleyTable[Int]): Boolean = {
    val rows = numericTable.table
    val identityRow = (0 until rows.length).toList
    rows.map(_.sorted).contains(identityRow)
  }

  def hasIdentityColumn(numericTable: CayleyTable[Int]): Boolean = {
    val columns = numericTable.table.transpose
    val identityColumn = (0 until columns.length).toList
    columns.map(_.sorted).contains(identityColumn)
  }

  def hasIdentityElement(numericTable: CayleyTable[Int]): Boolean = {
    val sorted = sortTable(numericTable).table
    val firstRow = sorted.head
    val firstColumn = sorted.map(_.head)

    val elems = (0 until firstColumn.length).toList

    firstRow == elems && firstColumn == elems
  }

  case class Triple(x: Int, y: Int, z: Int)

  object Triple {
    def apply(xs: List[Int]): Triple = Triple(xs(0), xs(1), xs(2))
  }

  // consider nxn table
  // we would need to retrieve all possible sets of 3 elements
  // and then check each set for associativity
  // TODO: Consider implementing Light's associativity test
  // https://en.wikipedia.org/wiki/Light%27s_associativity_test
  def isAssociative(numericTable: CayleyTable[Int]): Boolean = {
    val possibleMonoid = toFiniteMonoid(numericTable)
    val op = (x: Int) => (y: Int) => possibleMonoid.op(x,y)

    val triples = combinations(3)(possibleMonoid.elements).map(Triple(_))

    def tripleIsAssociative(t: Triple): Boolean = {
      val Triple(x, y, z) = t
      op(x)(op(y)(z)) == op(op(x)(y))(z)
    }

    triples.forall(tripleIsAssociative)
  }

}
