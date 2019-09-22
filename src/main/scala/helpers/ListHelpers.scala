package helpers

object ListHelpers {
  type NestedList[A] = List[List[A]]

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] =
    xs.zip(ys).map(z => f(z._1,z._2))

  def deepMap[A,B](f: A => B)(xss: NestedList[A]): NestedList[B] =
    xss.map(_.map(f))

  def deepZipWith[A,B,C](xss: NestedList[A], yss: NestedList[B])(f: (A,B) => C): NestedList[C] =
    zipWith(xss,yss)(zipWith(_,_)(f))

  // this is obscenely dangerous because if you call it with something that never returns to its initial state you die
  @annotation.tailrec
  def obtainCycle[A](init: A, f: A => A, res: List[A]): List[A] = res match {
    case Nil => obtainCycle(init, f, List(init))
    case h :: Nil => obtainCycle(init, f, f(h) :: (h :: Nil))
    case h :: _ => {
      val next = f(h)
      if (next == init) res else obtainCycle(init, f, next :: res)
    }
  }

  def combinations[A](n: Int)(xs: List[A]): List[List[A]] =
    n match {
      case 0 => List(Nil)
      case _ => for {
        x <- xs
        ys <- combinations(n - 1)(xs)
      } yield x :: ys
    }

  def getNCombinations[A](n: Int)(xs: List[A]): List[List[A]] =
    xs match {
      case _ :: _ if n == 1 => xs.map(List(_))
      case hd :: tl =>
        getNCombinations(n - 1)(tl).map(hd :: _) ::: getNCombinations(n)(tl)
      case _ => Nil
    }
}
