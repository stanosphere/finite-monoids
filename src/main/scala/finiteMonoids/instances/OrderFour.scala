package finiteMonoids.instances

import datastructures.Matrix
import finiteMonoids.FiniteMonoid

sealed trait Quad
sealed case class Q0() extends Quad // Identity
sealed case class Q1() extends Quad
sealed case class Q2() extends Quad
sealed case class Q3() extends Quad

object OrderFour extends App {
  val cyclicGroup: FiniteMonoid[Quad] = new FiniteMonoid[Quad] {
    // yes I've written out all 16 cases, yes it was dumb
    def op(x: Quad, y: Quad): Quad = (x,y) match {
      case (Q0(), Q0()) => Q0()
      case (Q0(), Q1()) => Q1()
      case (Q0(), Q2()) => Q2()
      case (Q0(), Q3()) => Q3()
      case (Q1(), Q0()) => Q1()
      case (Q1(), Q1()) => Q2()
      case (Q1(), Q2()) => Q3()
      case (Q1(), Q3()) => Q0()
      case (Q2(), Q0()) => Q2()
      case (Q2(), Q1()) => Q3()
      case (Q2(), Q2()) => Q0()
      case (Q2(), Q3()) => Q1()
      case (Q3(), Q0()) => Q3()
      case (Q3(), Q1()) => Q0()
      case (Q3(), Q2()) => Q1()
      case (Q3(), Q3()) => Q2()
    }
    def zero: Quad = Q0()
    def elements: List[Quad] = List(Q0(), Q1(), Q2(), Q3())
  }

  val vierergruppe: FiniteMonoid[Quad] = new FiniteMonoid[Quad] {
    def op(x: Quad, y: Quad): Quad = (x,y) match {
      case (Q0(), Q0()) => Q0()
      case (Q0(), Q1()) => Q1()
      case (Q0(), Q2()) => Q2()
      case (Q0(), Q3()) => Q3()
      case (Q1(), Q0()) => Q1()
      case (Q1(), Q1()) => Q0()
      case (Q1(), Q2()) => Q3()
      case (Q1(), Q3()) => Q2()
      case (Q2(), Q0()) => Q2()
      case (Q2(), Q1()) => Q3()
      case (Q2(), Q2()) => Q0()
      case (Q2(), Q3()) => Q1()
      case (Q3(), Q0()) => Q3()
      case (Q3(), Q1()) => Q2()
      case (Q3(), Q2()) => Q1()
      case (Q3(), Q3()) => Q0()
    }
    def zero: Quad = Q0()
    def elements: List[Quad] = List(Q0(), Q1(), Q2(), Q3())
  }

  val vierergruppeMatrixRep: FiniteMonoid[Matrix[Int]] = new FiniteMonoid[Matrix[Int]] {
    override def elements: List[Matrix[Int]] = List(
      Matrix(List(1,0), List(0,1)),
      Matrix(List(1,0), List(0,-1)),
      Matrix(List(-1,0), List(0,1)),
      Matrix(List(-1,0), List(0,-1))
    )
    override def op(a1: Matrix[Int], a2: Matrix[Int]): Matrix[Int] = a1 * a2
    override def zero: Matrix[Int] = Matrix.identityInt(2)
  }

  vierergruppe
    .computeCayleyTable
    .toNumericTable
    .show()

  vierergruppeMatrixRep
    .computeCayleyTable
    .toNumericTable
    .show()
}
