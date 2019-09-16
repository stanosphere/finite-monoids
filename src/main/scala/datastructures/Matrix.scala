package datastructures

import Mult.MultOps
import helpers.ListHelpers.{zipWith,deepZipWith,deepMap}
import Matrix.dotProduct

case class Matrix[A: Mult](elems: List[List[A]]) {
  def *(rightMatrix: Matrix[A]): Matrix[A] = {
    val leftMatrix = Matrix { elems }
    val (m,n) = size
    val indices = for { i <- 0 until m; j <- 0 until n } yield (i,j)
    indices.foldLeft(leftMatrix)((productMatrix, ij) => {
      val (i,j) = ij
      val newElem = dotProduct(
        leftMatrix.getRow(i),
        rightMatrix.getColumn(j)
      )
      productMatrix.update(i,j)(newElem)
    })
  }

  def +(m2: Matrix[A]): Matrix[A] = Matrix {
    deepZipWith(elems, m2.elems)(_+_)
  }

  def map[B: Mult](f: A => B): Matrix[B] = Matrix {
    deepMap(f)(elems)
  }

  def transpose: Matrix[A] = Matrix {
    elems.transpose
  }

  def getRow(i: Int): List[A] = elems(i)
  def getColumn(j: Int): List[A] = elems.map(_(j))
  def getElem(i: Int, j: Int): A = getRow(i)(j)

  def update(i: Int, j: Int)(x: A): Matrix[A] = Matrix {
    val newRow = getRow(i).updated(j, x)
    elems.updated(i,newRow)
  }

  def size: (Int, Int) = (elems.length, elems.head.length)
  def isSquare: Boolean = size._1 == size._2

  def show(): Unit = {
    println("Matrix:")
    elems.foreach(row => {
      println(row.map(_.toString).reduce((x,y) => s"${x}, ${y}"))
    })
  }
}

object Matrix extends App {
  def dotProduct[A: Mult](xs: List[A], ys: List[A]): A =
    zipWith(xs, ys)(_*_).reduce(_+_)

  def fill[A: Mult](n: Int)(x: A) = Matrix {
    List.fill(n)(List.fill(n)(x))
  }

  def apply[A: Mult](rows: List[A]*): Matrix[A] = Matrix {
    rows.toList
  }

  def empty(n: Int): Matrix[Int] = fill(n)(0)

  def identityInt(n: Int): Matrix[Int] =
    (0 until n).foldLeft(empty(n))(
      (m,i) => m.update(i,i)(1)
    )
}
