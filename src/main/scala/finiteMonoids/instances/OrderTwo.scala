package finiteMonoids.instances

import datastructures.{Complex, Matrix}
import finiteMonoids.FiniteMonoid

sealed trait Pair
sealed case class E() extends Pair
sealed case class A() extends Pair

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

  val booleanOr: FiniteMonoid[Boolean] = new FiniteMonoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x || y
    def zero: Boolean = false
    def elements: List[Boolean] = List(true, false)
  }

  val booleanAnd: FiniteMonoid[Boolean] = new FiniteMonoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x && y
    def zero: Boolean = true
    def elements: List[Boolean] = List(true, false)
  }

  val complex: FiniteMonoid[Complex] = new FiniteMonoid[Complex] {
    def elements: List[Complex] = List(Complex(1,0), Complex(-1, 0))
    def op(x: Complex, y: Complex): Complex = x * y
    def zero: Complex = Complex(1,0)
  }

  val idempotentMatrices: FiniteMonoid[Matrix[Int]] = new FiniteMonoid[Matrix[Int]] {
    override def elements: List[Matrix[Int]] = {
      val idempotentMatrix = Matrix(List(3, -6), List(1, -2)) // A * A == A
      List(Matrix.identityInt(2), idempotentMatrix)
    }
    override def op(a1: Matrix[Int], a2: Matrix[Int]): Matrix[Int] = a1 * a2
    override def zero: Matrix[Int] = Matrix.identityInt(2)
  }
}
