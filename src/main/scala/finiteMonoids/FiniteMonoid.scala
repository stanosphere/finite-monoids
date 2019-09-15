package finiteMonoids

// TODO: Consider if a `Set` would be a better way to represent the elements of the monoid

/**
 * A [[Monoid]] combined with its finite list of elements
 * */
trait FiniteMonoid[A] extends Monoid[A] {
  def elements: List[A]

  def computeCayleyTable: CayleyTable[A] = CayleyTable {
    val allCombinations = for { x <- elements; y <- elements } yield op(x,y)
    allCombinations.grouped(elements.length).toList
  }
}

object FiniteMonoid {
  def areIsomorphic[A,B](x: FiniteMonoid[A], y: FiniteMonoid[B]): Boolean =
    CayleyTable.areIsomorphic(x.computeCayleyTable, y.computeCayleyTable)
}
