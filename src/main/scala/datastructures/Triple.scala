package datastructures

case class Triple(x: Int, y: Int, z: Int)

object Triple {
  def apply(xs: List[Int]): Triple = Triple(xs(0), xs(1), xs(2))
}
