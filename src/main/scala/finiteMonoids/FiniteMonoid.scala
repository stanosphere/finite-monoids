package finiteMonoids

import java.util.concurrent.{ExecutorService, Executors}

import helpers.MapHelpers.invert
import parallelism.Par
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
}

object FiniteMonoid {
  def areIsomorphic[A, B](x: FiniteMonoid[A], y: FiniteMonoid[B]): Boolean =
    CayleyTable.areIsomorphic(x.computeCayleyTable, y.computeCayleyTable)

  def reduceLeft[A](m: FiniteMonoid[A])(as: List[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldInParallel[A](m: Monoid[A])(as: IndexedSeq[A]): A = {
    val answerAsPar: Par[A] = Par.foldPar(m.zero)(m.op)(as)

    answerAsPar(Executors.newWorkStealingPool).get
  }

  def reduceLeftInCayleySpace[A](m: FiniteMonoid[A])(as: List[A]): A = {
    val cayleyTable = m computeCayleyTable
    val cayleyMonoid = cayleyTable toFiniteMonoid
    val aToInt: Map[A, Int] = cayleyTable getMapToNumericRep
    val intToA: Map[Int, A] = invert(aToInt)

    val ints = as map aToInt

    val intRes = ints.foldLeft(cayleyMonoid zero)(cayleyMonoid op)

    intToA(intRes)
  }
}
