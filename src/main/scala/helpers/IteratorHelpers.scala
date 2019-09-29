package helpers

object IteratorHelpers {
  def combinations[A](n: Int)(xs: List[A]): Iterator[List[A]] = n match {
    case 0 => Iterator(Nil)
    case _ => for {
      ys <- combinations(n - 1)(xs)
      x <- xs
    } yield x :: ys
  }
}
