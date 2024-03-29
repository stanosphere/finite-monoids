package datastructures

import datastructures.Matrix.dotProduct
import datastructures.Mult.MultOps
import helpers.ListHelpers.{allChoices, combinations, deepMap, deepZipWith, zipWith}

case class Matrix[A: Mult](elems: List[List[A]]) {
  def *(rightMatrix: Matrix[A]): Matrix[A] = {
    val leftMatrix = this
    val (m, n) = size
    val indices = for {i <- 0 until m; j <- 0 until n} yield (i, j)
    indices.foldLeft(leftMatrix)((productMatrix, ij) => {
      val (i, j) = ij
      val newElem = dotProduct(
        leftMatrix.getRow(i),
        rightMatrix.getColumn(j)
      )
      productMatrix.update(i, j)(newElem)
    })
  }

  def +(m2: Matrix[A]): Matrix[A] = Matrix {
    deepZipWith(elems, m2.elems)(_ + _)
  }

  def map[B: Mult](f: A => B): Matrix[B] = Matrix {
    deepMap(f)(elems)
  }

  def transpose: Matrix[A] = Matrix {
    elems.transpose
  }

  def getRow(i: Int): List[A] = elems(i)

  def getColumn(j: Int): List[A] = elems.map(_ (j))

  def getElem(i: Int, j: Int): A = getRow(i)(j)

  def getAllRows: List[List[A]] = {
    for {i <- 0 until size._1} yield getRow(i)
    }.toList

  def getAllColumns: List[List[A]] = {
    for {j <- 0 until size._2} yield getColumn(j)
    }.toList

  def update(i: Int, j: Int)(x: A): Matrix[A] = Matrix {
    val newRow = getRow(i).updated(j, x)
    elems.updated(i, newRow)
  }

  def size: (Int, Int) = (elems.length, elems.head.length)

  def isSquare: Boolean = size._1 == size._2

  override def toString: String = {
    val rows = elems
      .map(_.map(_.toString).reduce((x, y) => s"$x, $y"))
      .reduce(_ + "\n  " + _)
    s"""Matrix {
       |  $rows
       |}
       |""".stripMargin
  }

  def show(): Unit = {
    println("Matrix:")
    elems.foreach(row =>
      println(row.map(_.toString).reduce((x, y) => s"$x, $y"))
    )
  }
}

object Matrix {
  def dotProduct[A: Mult](xs: List[A], ys: List[A]): A =
    zipWith(xs, ys)(_ * _).reduce(_ + _)

  def fill[A: Mult](n: Int)(x: A) = Matrix {
    List.fill(n)(List.fill(n)(x))
  }

  def apply[A: Mult](rows: List[A]*): Matrix[A] = Matrix {
    rows.toList
  }

  def empty(n: Int): Matrix[Int] = fill(n)(0)

  def identityInt(n: Int): Matrix[Int] =
    (0 until n).foldLeft(empty(n))(
      (m, i) => m.update(i, i)(1)
    )

  // list will be n! long
  def getPermutationMatrices(n: Int): List[Matrix[Int]] = {
    val startingRow = 1 :: List.fill(n - 1)(0)
    val rows = startingRow.permutations.toList
    rows.permutations.toList.map(Matrix(_))
  }

  // produces a list of length 2 ^ n where n is the size of the matrix
  def getAllSignPermutations(permutationMatrix: Matrix[Int]): List[Matrix[Int]] =
    allChoices(
      permutationMatrix
        .getAllRows
        .map(row => List(row, row.map(-_)))
    )
      .map(Matrix(_))
}
