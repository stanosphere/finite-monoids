package helpers

import org.scalatest.FlatSpec
import ListHelpers._

class TestListHelperMethods extends FlatSpec {
  /**
   * List(List(1,2), List(3)) ->
   * List(List(1,3), List(2,3)
   */
  "allChoices" should "just work for this very simple case" in {
    val listOfChoices = List(List(1, 2), List(3))
    val expected = List(List(1, 3), List(2, 3))

    println(allChoices(List(List(1), List(2))))

    assert(allChoices(listOfChoices) === expected)
  }
}
