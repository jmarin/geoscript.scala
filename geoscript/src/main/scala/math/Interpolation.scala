package org.geoscript.support.math

trait Interpolation[V] extends (Double => V)

object Interpolation {
  case class Linear[V : VectorSpace](from: V, to: V) extends Interpolation[V] {
    private val vectorSpace = implicitly[VectorSpace[V]]
    import vectorSpace._

    def apply(i: Double): V = (from * i) + (to * (1d - i))
  }
}
