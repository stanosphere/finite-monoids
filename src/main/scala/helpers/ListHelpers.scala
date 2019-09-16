package helpers

object ListHelpers {
  type NestedList[A] = List[List[A]]

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] =
    xs.zip(ys).map(z => f(z._1,z._2))

  def deepMap[A,B](f: A => B)(xss: NestedList[A]): NestedList[B] =
    xss.map(_.map(f))

  def deepZipWith[A,B,C](xss: NestedList[A], yss: NestedList[B])(f: (A,B) => C): NestedList[C] =
    zipWith(xss,yss)(zipWith(_,_)(f))
}
