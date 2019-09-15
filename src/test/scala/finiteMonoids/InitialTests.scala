package finiteMonoids

import org.scalatest._

// I'm not entirely sure how I would like to structure this yet
// But a good starting point is verifying that my isomorphism detection is correct for order 2 monoids
class InitialTests extends FlatSpec {

  "A Stack" should "pop values in last-in-first-out order" in {
    assert(1 === 1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    assert(1 === 1)
  }
}
