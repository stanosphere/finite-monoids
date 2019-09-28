package finiteMonoids

import CayleyTable.{areIsomorphic, hasIdentityElement, sortRows, isAssociative}
import helpers.ListHelpers.{combinations, distinctWith}

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
  def getAllCayleyTables(n: Int): List[CayleyTable[Int]] = {
    // initialy the list will have a length of n ^ (n^2)
    val cayleys = combinations(n)(allRows(n))
      .distinct
      .map(CayleyTable(_))
      .filter(hasIdentityElement) // get rid of tables that have no identity elements
      .map(sortRows).distinct // get rid of duplicates after sorting

    val res = distinctWith(cayleys)(areIsomorphic).filter(isAssociative)
    println("final result", res.length)
    res
  }

  getAllCayleyTables(2).foreach(_.show())
}
