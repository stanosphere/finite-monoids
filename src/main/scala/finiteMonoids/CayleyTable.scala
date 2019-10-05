package finiteMonoids

import scala.math.Ordering.Implicits._
import helpers.ListHelpers.{combinations, deepMap}
import helpers.Tabulator.tabulateCayley
import datastructures.{Matrix, Triple}
import file.CSVLine

// this is a simple way of representing the structure of a finite monoid
// or, more generally, a finite Magma
// https://en.wikipedia.org/wiki/Cayley_table
case class CayleyTable[A](table: List[List[A]]) {
  def prettyPrint(): Unit =
    println(tabulateCayley(this.toSymbolic))

  def map[B](f: A => B): CayleyTable[B] = CayleyTable {
    deepMap(f)(table)
  }

  def toNumericTable: CayleyTable[Int] = CayleyTable {
    val representation = table.flatten.distinct.zipWithIndex.toMap[A, Int]
    deepMap(representation)(table)
  }

  def toSymbolic: CayleyTable[String] = {
    def toSymbolicElem(x: Int): String = (x + 97).toChar.toString

    this.toNumericTable map toSymbolicElem
  }

  def toCSVLine: CSVLine = CSVLine {
    this.toSymbolic.table.flatten.mkString(",")
  }

  def elems: List[A] = table.flatten.distinct
}

object CayleyTable {
  def sortColumns(c: CayleyTable[Int]): CayleyTable[Int] =
    CayleyTable {
      c.table.transpose.sorted.transpose
    }

  def sortRows(c: CayleyTable[Int]): CayleyTable[Int] =
    CayleyTable {
      c.table.sorted
    }

  def sortTable(c: CayleyTable[Int]): CayleyTable[Int] =
    (sortRows _ andThen sortColumns) (c)

  // need to compare one Cayley table with all permutations of the other
  // all we're trying to establish is if they have the same structure, not the same contents
  def areIsomorphic[A, B](as: CayleyTable[A], bs: CayleyTable[B]): Boolean = {
    val aNums = as.toNumericTable
    val bNums = bs.toNumericTable

    val sortedPermutations = getAllPermutations(aNums) map sortTable
    val sortedB = sortTable(bNums)

    sortedPermutations contains sortedB
  }

  // I believe this could be made polymorphic with a judicious use of `zipWithIndex`
  def getAllPermutations(as: CayleyTable[Int]): List[CayleyTable[Int]] = {
    val permutations = as.elems.permutations.toList
    val fs: List[Int => Int] = permutations map (perm => x => perm(x))

    fs map as.map
  }

  def toMagma(numericTable: CayleyTable[Int]): Magma[Int] = {
    val elems = numericTable.table.head
    // just need to look up entry in cayley table
    // for convenience I think I'll just turn it into a matrix
    val matrix = Matrix(numericTable.table)

    new Magma[Int] {
      def elements: List[Int] = elems

      def op(x: Int, y: Int): Int = matrix.getElem(x, y)
    }
  }

  def hasIdentityElement(numericTable: CayleyTable[Int]): Boolean = {
    val sorted = sortTable(numericTable).table
    val firstRow = sorted.head
    val firstColumn = sorted.map(_.head)

    val elems = (0 until firstColumn.length).toList

    firstRow == elems && firstColumn == elems
  }

  // consider nxn table
  // we would need to retrieve all possible sets of 3 elements
  // and then check each set for associativity
  // TODO: Consider implementing Light's associativity test
  // https://en.wikipedia.org/wiki/Light%27s_associativity_test
  def isAssociative(numericTable: CayleyTable[Int]): Boolean = {
    val magma = toMagma(numericTable)
    val op = (x: Int) => (y: Int) => magma.op(x, y)

    val triples = combinations(3)(magma.elements).map(Triple(_))

    def tripleIsAssociative(t: Triple): Boolean = {
      val Triple(x, y, z) = t
      op(x)(op(y)(z)) == op(op(x)(y))(z)
    }

    triples.forall(tripleIsAssociative)
  }

}
