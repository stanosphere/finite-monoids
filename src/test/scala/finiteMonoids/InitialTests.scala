package finiteMonoids

import org.scalatest._
import FiniteMonoid.areIsomorphic
import finiteMonoids.instances.OrderTwo.{
  groupMonoid,
  nonGroupMonoid,
  booleanAnd,
  booleanOr,
  complex
}

// I'm not entirely sure how I would like to structure this yet
// But a good starting point is verifying that my isomorphism detection is correct for order 2 monoids
class InitialTests extends FlatSpec {

  "The boolean monoids" should "be isomorphic" in {
    println(booleanOr.computeCayleyTable.showPermutations)
    println(booleanAnd.computeCayleyTable.showPermutations)

    assert(areIsomorphic(booleanOr, booleanAnd) === true)
  }

  "the group and non group monoids" should "be isomorphic" in {
    assert(areIsomorphic(nonGroupMonoid, groupMonoid) === false)
  }
}
