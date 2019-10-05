package datastructures

case class Complex(re: Double, im: Double) {
  def +(y: Complex): Complex = Complex(re + y.re, im + y.im)

  def -(y: Complex): Complex = Complex(re - y.re, im - y.im)

  def *(y: Complex): Complex = Complex(
    re * y.re - im * y.im,
    re * y.im + im * y.re
  )

  def conjugate(): Complex = Complex(re, -im)

  def mag(): Double = Math.hypot(re, im)
}

object Complex extends App {
  println(Complex(1, 2))
}
