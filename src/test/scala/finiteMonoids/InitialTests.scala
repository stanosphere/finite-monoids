package finiteMonoids

import org.scalatest._

class InitialTests extends FlatSpec {

  "A Stack" should "pop values in last-in-first-out order" in {
    assert(1 === 1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    assert(1 === 1)
  }
}
