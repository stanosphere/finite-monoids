package finiteMonoids

import helpers.MapHelpers.invert
import parallelism.Par
import parallelism.Par.toParOps
import parallelism.Par.Par

// TODO: Consider if a `Set` would be a better way to represent the elements of the monoid

/**
 * A [[Monoid]] combined with its finite list of elements
 **/
trait FiniteMonoid[A] extends Monoid[A] {
  def elements: List[A]

  def computeCayleyTable: CayleyTable[A] = CayleyTable {
    val allCombinations = for {x <- elements; y <- elements} yield op(x, y)
    allCombinations.grouped(elements.length).toList
  }

  def isAssociative: Boolean =
    CayleyTable.isAssociative(computeCayleyTable.toNumericTable)

  def hasIdentityElement: Boolean =
    CayleyTable.hasIdentityElement(computeCayleyTable.toNumericTable)

  def isClosed: Boolean = {
    val computedElements = computeCayleyTable.table.flatten.distinct.toSet
    elements.toSet == computedElements
  }

  def isMonoid: Boolean = isAssociative && hasIdentityElement && isClosed

  def *[B](b: FiniteMonoid[B]): FiniteMonoid[(A, B)] = {
    val aOp: (A, A) => A = this.op
    val aZero = this.zero
    val aElements = this.elements

    new FiniteMonoid[(A, B)] {
      def elements: List[(A, B)] =
        for {a <- aElements; b <- b.elements} yield (a, b)

      def op(x: (A, B), y: (A, B)): (A, B) =
        (aOp(x._1, y._1), b.op(x._2, y._2))

      def zero: (A, B) = (aZero, b.zero)
    }
  }
}

object FiniteMonoid {
  def areIsomorphic[A, B](x: FiniteMonoid[A], y: FiniteMonoid[B]): Boolean =
    CayleyTable.areIsomorphic(x.computeCayleyTable, y.computeCayleyTable)

  def reduceLeft[A](m: FiniteMonoid[A])(as: List[A]): A =
    as.foldLeft(m.zero)(m.op)

  // the fact that we can do this follows from the associativity and identity laws
  def foldInParallel[A](m: Monoid[A])(as: IndexedSeq[A]): A = {
    // here a Par just represents a unit of parallelism
    val res: Par[A] = Par.foldPar(m.zero)(m.op)(as)
    res.unsafeRun
  }

  def reduceLeftInCayleySpace[A](m: FiniteMonoid[A])(as: List[A]): A = {
    val cayleyTable = m.computeCayleyTable
    val cayleyMonoid = cayleyTable.toFiniteMonoid
    val aToInt: Map[A, Int] = cayleyTable.getMapToNumericRep
    val intToA: Map[Int, A] = invert(aToInt)

    val ints = as map aToInt

    val intRes = ints.foldLeft(cayleyMonoid.zero)(cayleyMonoid.op)

    intToA(intRes)
  }
}
