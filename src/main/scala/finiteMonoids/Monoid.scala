package finiteMonoids

/**
 * A Monoid is defined here as an associative operation, `op`, combined with an identity element, `zero`
 * I have chosen to define my own Monoid rather than use cats or similar
 * This is because this project is purely for learning scala and functional programming
 * And I feel that the best way to do this is try to do as much from scratch as possible
 **/
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}
