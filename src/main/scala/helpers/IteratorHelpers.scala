package helpers

object IteratorHelpers {
  def combinations[A](n: Int)(xs: List[A]): Iterator[List[A]] = n match {
    case 0 => Iterator(Nil)
    case _ => for {
      ys <- combinations(n - 1)(xs)
      x <- xs
    } yield x :: ys
  }

  def distinctWith[A](xs: Iterator[A])(f: (A, A) => Boolean): Iterator[A] = {
    xs.foldLeft(Iterator(): Iterator[A])((distinctXs, x) => {
      val (forTraversal, forAnswer) = distinctXs.duplicate
      if (forTraversal forall (!f(_,x))) forAnswer ++ Iterator(x)
      else forAnswer
    })
  }
}
