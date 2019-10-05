package finiteMonoids

// the most general closed group-like algebraic structure
trait Magma[A] {
  def op(a1: A, a2: A): A

  def elements: List[A]
}
