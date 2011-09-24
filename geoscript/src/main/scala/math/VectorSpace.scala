package org.geoscript.support.math
import java.awt.Color

trait VectorSpace[V] {
  def plus(a: V, b: V): V
  def times(a: V, b: Double): V

  implicit def enableOperators(a: V) = new Operators(a)

  class Operators(a: V) {
    def +(b: V): V = plus(a, b)
    def *(b: Double): V = times(a, b)
  }
}

object VectorSpace {
  implicit val doublesAreAVectorSpace = 
    new VectorSpace[Double] {
      def plus(a: Double, b: Double) = a + b
      def times(a: Double, b: Double) = a * b
    }

  implicit val colorsAreAVectorSpace =
    new VectorSpace[Color] {
      def plus(x: Color, y: Color) = {
        val xs = x.getComponents(Array.ofDim[Float](4))
        val ys = y.getComponents(Array.ofDim[Float](4))
        val res = (xs zip ys) map { case (a, b) => a + b }
        val Array(r, g, b, a) = res
        new java.awt.Color(r, g, b, a)
      }

      def times(x: Color, y: Double) = {
        val xs = x.getComponents(Array.ofDim[Float](4))
        val res = xs map { _ * y.toFloat }
        val Array(r, g, b, a) = res
        new java.awt.Color(r, g, b, a)
      }
    }
}
