package finiteMonoids

import finiteMonoids.instances.OrderTwo.{groupMonoid, nonGroupMonoid}
import finiteMonoids.instances.OrderFour.vierergruppe

object MultiplicationPlayingAround extends App {
  val klein = groupMonoid * groupMonoid

  val hello = FiniteMonoid.areIsomorphic(klein, vierergruppe)

  println(hello)
}
