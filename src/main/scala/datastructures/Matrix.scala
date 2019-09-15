package datastructures

import Mult.MultOps
import helpers.ListHelpers.zipWith

case class Matrix[A: Mult](elems: List[List[A]]) {
//  def *(m2: Matrix): Matrix = {
//
//  }
}

object Matrix extends App {
  def dotProduct[A: Mult](xs: List[A], ys: List[A]): A =
    zipWith(xs, ys)(_*_).reduce(_+_)
}
