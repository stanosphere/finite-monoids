package finiteMonoids.instances

import finiteMonoids.FiniteMonoid.areIsomorphic
import finiteMonoids.instances.OrderFour._
import org.scalatest._

class OrderFourTests extends FlatSpec {
  "all representation of the klein four group" should "be isomorphic" in {
    assert(areIsomorphic(vierergruppe, vierergruppeMatrixRep) === true)
  }
}
