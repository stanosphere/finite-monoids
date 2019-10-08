package helpers

object MapHelpers {
  // This will only work on maps where the mapping is 1 <-> 1
  // and all keys are unique
  def invert[A, B](m: Map[A, B]): Map[B, A] =
    m.map(_.swap)
}
