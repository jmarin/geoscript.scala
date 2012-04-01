package org.geoscript

import collection.JavaConversions._
import support.logic.Knowledge
import org.opengis.{ filter => ogc }

package object geocss {
  val filters = org.geotools.factory.CommonFactoryFinder.getFilterFactory
  // should produce a seq of all the simple terms in the given (possibly
  // complex) selector.
  def flatten(sel: Selector): Seq[Selector] =
    sel match {
      case And(children) => (children flatMap flatten)
      case Or(children) => (children flatMap flatten)
      case sel => Seq(sel)
    }

  // should produce an equivalent selector to the input with possibly fewer
  // total terms
  def simplify(sel: Selector): Selector = {
    Knowledge.Oblivion[Selector].reduce(sel)
  }

  // should produce an OGC filter expressing the constraints in a selector
  // that can be expressed in OGC filters.
  def realize(sel: Selector): Option[ogc.Filter] =
    sel match {
      case And(xs) =>
        (xs flatMap realize) match {
          case Nil => None
          case Seq(f) => Some(f)
          case fs  => Some(filters.and(fs))
        }
      case Or(xs) =>
        (xs flatMap realize) match {
          case Nil => None
          case Seq(f) => Some(f)
          case fs  => Some(filter.FilterOps.anyOf(fs))
        }
      case Not(x) =>
        for (f <- realize(x)) yield filter.FilterOps.negate(f)
      case (f: DataSelector) => f.filterOpt
      case _ => None
    }

  // make a composite filter that matches features matched by all of the
  // given filters (logical AND)
  def allOf(sels: Seq[Selector]): Selector = And(sels)
}
