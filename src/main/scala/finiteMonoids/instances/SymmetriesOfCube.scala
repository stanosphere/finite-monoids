package finiteMonoids.instances

import datastructures.Matrix
import datastructures.Matrix.{getAllSignPermutations, getPermutationMatrices}
import finiteMonoids.FiniteMonoid

// https://en.wikipedia.org/wiki/Octahedral_symmetry

// Take the set of all 3x3 permutation matrices
// and assign a + sign or a - sign to each of the three 1s.
// There are 6 matrices x 8 sign permutations = 48 matrices in total giving the full octahedral group.
// There are exactly 24 matrices with determinant = +1
// and these are the rotation matrices of the chiral octahedral group.
// The other 24 matrices correspond to a reflection or inversion.

object SymmetriesOfCube {

  val CubeMonoid: FiniteMonoid[Matrix[Int]] = new FiniteMonoid[Matrix[Int]] {
    override def elements: List[Matrix[Int]] =
      getPermutationMatrices(3) flatMap getAllSignPermutations

    println(elements.length)

    override def op(a1: Matrix[Int], a2: Matrix[Int]): Matrix[Int] = a1 * a2

    override def zero: Matrix[Int] = Matrix.identityInt(3)
  }
}
