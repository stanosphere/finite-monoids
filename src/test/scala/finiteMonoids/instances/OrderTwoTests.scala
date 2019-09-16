package finiteMonoids.instances

import finiteMonoids.FiniteMonoid.areIsomorphic
import finiteMonoids.instances.OrderTwo._
import org.scalatest._

// I'm not entirely sure how I would like to structure this yet
// But a good starting point is verifying that my isomorphism detection is correct for order 2 monoids
class OrderTwoTests extends FlatSpec {

  "The boolean monoids" should "be isomorphic" in {
    assert(areIsomorphic(booleanOr, booleanAnd) === true)
  }

  "the group and non group monoids" should "not be isomorphic" in {
    assert(areIsomorphic(nonGroupMonoid, groupMonoid) === false)
  }

  "the idempotent matrices monoid and the non group monoid" should "be isomorphic" in {
    assert(areIsomorphic(nonGroupMonoid, idempotentMatrices) === true)
  }
}
