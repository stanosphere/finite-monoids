package datastructures
// https://scalac.io/typeclasses-in-scala/

trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A](implicit sh: Show[A]): Show[A] = sh

  def show[A: Show](a: A): String = Show[A].show(a)

  implicit class ShowOps[A: Show](a: A) {
    def show: String = Show[A].show(a)
  }

  implicit val intCanShow: Show[Int] =
    (int: Int) => s"int $int"
}

object Demo extends App {
  println()
}
