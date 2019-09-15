package helpers

object ListHelpers {
  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] =
    xs.zip(ys).map(z => f(z._1,z._2))

  def deepMap[A,B](f: A => B)(xss: List[List[A]]): List[List[B]] =
    xss.map(_.map(f))
}
