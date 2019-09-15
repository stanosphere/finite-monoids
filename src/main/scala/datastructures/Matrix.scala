package datastructures

import Mult.MultOps

case class Matrix[A: Mult](elems: List[List[A]]) {
//  def *(m2: Matrix): Matrix = {
//
//  }
}

object Matrix extends App {
  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] =
    xs.zip(ys).map(z => f(z._1,z._2))

  def dotProduct[A: Mult](xs: List[A], ys: List[A]): A =
    zipWith(xs, ys)(_*_).reduce(_+_)
}
