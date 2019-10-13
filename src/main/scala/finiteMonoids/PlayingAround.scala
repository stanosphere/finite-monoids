package finiteMonoids

import scala.util.Random
import finiteMonoids.instances.SymmetriesOfCube.CubeMonoid
import helpers.Timed

object PlayingAround extends App {
  val cubeMonoid = CubeMonoid
  val random = Random

  cubeMonoid.elements.take(10).foreach(_.show())
  cubeMonoid.computeCayleyTable.prettyPrint()

  def getRandomCombinationOfLengthN[A](elems: List[A])(n: Int): List[A] = {
    def nextA = elems(Random.nextInt(elems.length))
    List.fill(n)(nextA)
  }

  val tenElements = getRandomCombinationOfLengthN(cubeMonoid.elements)(10)

  Timed("10 normal way") {
    FiniteMonoid.reduceLeft(cubeMonoid)(tenElements)
  }

  Timed("10 Cayley way") {
    FiniteMonoid.reduceLeftInCayleySpace(cubeMonoid)(tenElements)
  }

  val oneMillionElements =
    getRandomCombinationOfLengthN(cubeMonoid.elements)(1000000)

  Timed("1,000,000 normal way") {
    FiniteMonoid.reduceLeft(cubeMonoid)(oneMillionElements)
  }

  Timed("1,000,000 Cayley way") {
    FiniteMonoid.reduceLeftInCayleySpace(cubeMonoid)(oneMillionElements)
  }



}
