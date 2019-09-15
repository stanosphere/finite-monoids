package datastructures

trait Mult[A] {
  def *(x: A, y: A): A
  def +(x: A, y: A): A
}

object Mult {
  def apply[A](implicit mult: Mult[A]): Mult[A] = mult

  def *[A: Mult](x: A, y: A): A = Mult[A] * (x,y)
  def +[A: Mult](x: A, y: A): A = Mult[A] + (x,y)

  implicit class MultOps[A: Mult](x: A) {
    def *(y: A): A = Mult[A] * (x,y)
    def +(y: A): A = Mult[A] + (x,y)
  }

  // instances
  implicit val complex: Mult[Complex] =
    new Mult[Complex] {
      def *(x: Complex, y: Complex): Complex = x * y
      def +(x: Complex, y: Complex): Complex = x + y
    }

  implicit val int: Mult[Int] =
    new Mult[Int] {
      def *(x: Int, y: Int): Int = x * y
      def +(x: Int, y: Int): Int = x + y
    }
}