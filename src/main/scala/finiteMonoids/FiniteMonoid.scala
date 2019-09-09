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

object CheckIsomorphisms extends App {
  import finiteMonoids.instances.OrderTwo._
  val nonGroupCayley = nonGroupMonoid.computeCayleyTable.toSortedNumericTable
  val nonGroupCayley1 = nonGroupMonoid.computeCayleyTable.toSortedNumericTable

//  val booleanCayley = Stanoid(Monoid.booleanOr, List(true, false)).computeCayleyTable.toSortedNumericTable
//  val booleanCayley1 = Stanoid(Monoid.booleanOr, List(false, true)).computeCayleyTable.toSortedNumericTable

  CayleyTable.getAllPermutations(nonGroupCayley).foreach(_.show)
}
