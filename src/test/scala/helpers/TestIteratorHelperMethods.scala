package helpers

import org.scalatest.FlatSpec
import IteratorHelpers._


class TestIteratorHelperMethods extends FlatSpec {
  "distinctWith" should "just behave like `distinct` if I dive it the equality function" in {
    val xs = Iterator(1,2,3)
    val equals = (x: Any, y: Any) => x == y
    val res = distinctWith(xs)(equals).toList
    assert(res === List(1,2,3))

    val ys = Iterator(1,1,2,2,3,3)
    val zs = Iterator(1,2,3,1,2,3)

    assert(distinctWith(ys)(equals).toList === List(1,2,3))
    assert(distinctWith(zs)(equals).toList === List(1,2,3))
  }
}
