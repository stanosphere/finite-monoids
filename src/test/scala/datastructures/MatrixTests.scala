package datastructures

import org.scalatest.FlatSpec

class MatrixTests extends FlatSpec {
  "Matrix multiplication" should "work for integers" in {
    val m1 = Matrix(
      List(2, -1),
      List(1,  3)
    )

    val m2 = Matrix(
      List(1, 2),
      List(3, 4)
    )

    assert(m1 * m2 === Matrix(
        List(-1, 0),
        List(10, 14)
      )
    )

    assert(m2 * m1 === Matrix(
        List(4, 5),
        List(10, 9)
      )
    )
  }

  "Matrix multiplication" should "work for complex numbers" in {
    val m1 = Matrix(
      List(Complex(1, -1), Complex(1, 0)),
      List(Complex(1, 3),  Complex(3, 4))
    )

    val m2 = Matrix(
      List(Complex(2, 3), Complex(-10, 3)),
      List(Complex(1, -5),  Complex(6, -2))
    )

    println(m1 * m2)
    println(m2 * m1)

    assert(m1 * m2 === Matrix(
        List(Complex(6.0,-4.0), Complex(-1.0,11.0)),
        List(Complex(16.0,-2.0), Complex(7.0,-9.0))
      )
    )

    assert(m2 * m1 === Matrix(
        List(Complex(-14.0,-26.0), Complex(-40.0,-28.0)),
        List(Complex(8.0,10.0), Complex(27.0,13.0))
      )
    )
  }
}
