package finiteMonoids

import CayleyTable._
import helpers.ListHelpers.combinations
import helpers.IteratorHelpers
import helpers.IteratorHelpers.distinctWith
import file.CSV
import file.SaveFile.saveCsv

// http://oeis.org/A058129
// 0, 1, 2, 7, 35, 228, 2237, 31559, 1668997

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

  // this might work best as an iterator
  // I've ordered the operations from least complicated to most complicated
  def getAllCayleyTables(n: Int): List[CayleyTable[Int]] = {
    // initially the list will have a length of n ^ (n^2)
    val cayleys = IteratorHelpers.combinations(n)(allRows(n))
      .distinct
      .map(CayleyTable(_))
      .filter(hasIdentityElement) // get rid of tables that have no identity elements
      .map(sortRows).distinct // get rid of duplicates after sorting
      .filter(isAssociative) // get rid of non associative tables

    val res = distinctWith(cayleys)(areIsomorphic).toList.map(sortTable) // remove isomorphic tables
    println("final result", res.length)
    val asCsv = CSV(Nil, res.map(_.toCSVLine))
    saveCsv(s"monoids/order$n.csv")(asCsv)
    res
  }

  getAllCayleyTables(3).foreach(_.prettyPrint())
}

//0, 1, 2
//1, 1, 1
//2, 1, 1
//
//0, 1, 2
//1, 2, 2
//2, 2, 2
