package finiteMonoids.instances

import finiteMonoids.FiniteMonoid

trait Pair
case class E() extends Pair
case class A() extends Pair

object OrderTwo {
  // this monoid is also a representation of the finite simple group of order 2
  val groupMonoid: FiniteMonoid[Pair] = new FiniteMonoid[Pair] {
    def op(x: Pair, y: Pair): Pair = (x,y) match {
      case (E(), a) => a
      case (a, E()) => a
      case (A(), A()) => E()
    }
    def zero: Pair = E()
    def elements: List[Pair] = List(E(), A())
  }

  val nonGroupMonoid: FiniteMonoid[Pair] = new FiniteMonoid[Pair] {
    def op(x: Pair, y: Pair): Pair = (x,y) match {
      case (E(), a) => a
      case (a, E()) => a
      case (A(), A()) => A()
    }
    def zero: Pair = E()
    def elements: List[Pair] = List(E(), A())
  }
}
