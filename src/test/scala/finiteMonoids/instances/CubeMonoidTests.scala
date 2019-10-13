package finiteMonoids.instances

import org.scalatest.FlatSpec
import finiteMonoids.instances.SymmetriesOfCube.CubeMonoid

class CubeMonoidTests extends FlatSpec {
  "the CubeMonoid" should "be a monoid" in {
    assert(CubeMonoid.isAssociative === true)
    assert(CubeMonoid.hasIdentityElement === true)
    assert(CubeMonoid.isClosed === true)
  }
}
